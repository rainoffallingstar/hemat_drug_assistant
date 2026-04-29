#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

PYTHON_BIN="${PYTHON_BIN:-${REPO_ROOT}/.venv312/bin/python}"
RSCONNECT_NICKNAME="${RSCONNECT_NICKNAME:-rainoffallingstar}"
RSCONNECT_ENTRYPOINT="${RSCONNECT_ENTRYPOINT:-app}"
RSCONNECT_TITLE="${RSCONNECT_TITLE:-hemat_drug_assistant}"
RSCONNECT_NEW="${RSCONNECT_NEW:-0}"
RSCONNECT_STAGE_PREFIX="${RSCONNECT_STAGE_PREFIX:-${RSCONNECT_TITLE}-shinyapps}"
RSCONNECT_NO_VERIFY="${RSCONNECT_NO_VERIFY:-1}"
RSCONNECT_DEPLOY_RETRIES="${RSCONNECT_DEPLOY_RETRIES:-3}"

resolve_app_id() {
  "${PYTHON_BIN}" - "${REPO_ROOT}" "${RSCONNECT_TITLE}" <<'PY'
import json
import pathlib
import sys

repo_root = pathlib.Path(sys.argv[1])
title = sys.argv[2]
metadata_path = repo_root / "rsconnect-python" / f"{title}.json"
if not metadata_path.exists():
    raise SystemExit(0)

with metadata_path.open("r", encoding="utf-8") as f:
    data = json.load(f)

entry = data.get("https://api.shinyapps.io") or {}
app_id = entry.get("app_id")
if app_id:
    print(app_id)
PY
}

RSCONNECT_APP_ID="${RSCONNECT_APP_ID:-$(resolve_app_id)}"

if [[ ! -x "${PYTHON_BIN}" ]]; then
  echo "Python interpreter not found or not executable: ${PYTHON_BIN}" >&2
  exit 1
fi

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/${RSCONNECT_STAGE_PREFIX}.XXXXXX")"
LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/${RSCONNECT_STAGE_PREFIX}-logs.XXXXXX")"
cleanup() {
  rm -rf "${TMP_DIR}"
  rm -rf "${LOG_DIR}"
}
trap cleanup EXIT

echo "[deploy] Building frontend"
(
  cd "${REPO_ROOT}/ui"
  npm run build
)

echo "[deploy] Staging runtime bundle in ${TMP_DIR}"
mkdir -p "${TMP_DIR}/bin" "${TMP_DIR}/data" "${TMP_DIR}/www"
cp "${REPO_ROOT}/app.py" "${TMP_DIR}/"
cp "${REPO_ROOT}/requirements.txt" "${TMP_DIR}/"
cp "${REPO_ROOT}/pyproject.toml" "${TMP_DIR}/"
cp -R "${REPO_ROOT}/bin/." "${TMP_DIR}/bin/"
cp -R "${REPO_ROOT}/data/." "${TMP_DIR}/data/"
cp -R "${REPO_ROOT}/ui/dist/." "${TMP_DIR}/www/"

echo "[deploy] Deploying staged bundle to shinyapps.io"
deploy_cmd=(
  "${PYTHON_BIN}" -m rsconnect.main deploy shiny "${TMP_DIR}"
  -n "${RSCONNECT_NICKNAME}"
  -e "${RSCONNECT_ENTRYPOINT}"
  -t "${RSCONNECT_TITLE}"
  -v
)

if [[ -n "${RSCONNECT_APP_ID}" ]]; then
  if [[ "${RSCONNECT_NEW}" == "1" ]]; then
    echo "[deploy] Creating a new shinyapps.io app"
    deploy_cmd+=(-N)
  else
    echo "[deploy] Replacing shinyapps.io app id ${RSCONNECT_APP_ID}"
    deploy_cmd+=(-a "${RSCONNECT_APP_ID}")
  fi
elif [[ "${RSCONNECT_NEW}" == "1" ]]; then
  echo "[deploy] Creating a new shinyapps.io app"
  deploy_cmd+=(-N)
fi

if [[ "${RSCONNECT_NO_VERIFY}" == "1" ]]; then
  echo "[deploy] Skipping rsconnect verify step"
  deploy_cmd+=(--no-verify)
fi

attempt=1
while (( attempt <= RSCONNECT_DEPLOY_RETRIES )); do
  echo "[deploy] Publish attempt ${attempt}/${RSCONNECT_DEPLOY_RETRIES}"
  log_file="${LOG_DIR}/rsconnect-deploy-${attempt}.log"
  set +e
  "${deploy_cmd[@]}" 2>&1 | tee "${log_file}"
  status=${PIPESTATUS[0]}
  set -e

  if [[ ${status} -eq 0 ]]; then
    echo "[deploy] shinyapps.io publish completed"
    exit 0
  fi

  if grep -q "UNEXPECTED_EOF_WHILE_READING" "${log_file}" || grep -q "EOF occurred in violation of protocol" "${log_file}"; then
    if (( attempt < RSCONNECT_DEPLOY_RETRIES )); then
      echo "[deploy] rsconnect hit SSL EOF while polling shinyapps.io; retrying in 5s"
      sleep 5
      ((attempt++))
      continue
    fi
  fi

  echo "[deploy] shinyapps.io publish failed"
  exit "${status}"
done
