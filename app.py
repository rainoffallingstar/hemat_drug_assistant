"""
app.py — shinyapps.io entry point for mellon
Proxies all HTTP traffic to the Go server before Shiny can inject its client scripts.
"""
import stat
import subprocess
import time
import os
from pathlib import Path

import httpx
import shiny

ROOT = Path(__file__).parent
GO_PORT = 18080
GO_URL = f"http://127.0.0.1:{GO_PORT}"
GO_BIN = ROOT / "bin/mellon-api"
GO_PROCESS = None
GO_READY = False
GO_START_ERROR = None
REQUEST_LOG_LIMIT = 20
DEFAULT_APP_PREFIX = f"/{ROOT.name}"


def log(message: str) -> None:
    print(f"[mellon] {message}", flush=True)


def normalized_app_prefix() -> str:
    prefix = os.getenv("MELLON_URL_PREFIX", DEFAULT_APP_PREFIX).strip()
    if not prefix:
        return ""
    if not prefix.startswith("/"):
        prefix = f"/{prefix}"
    return prefix.rstrip("/")


def upstream_path_for_scope(scope) -> tuple[str, str, str]:
    root_path = scope.get("root_path", "") or ""
    path = scope.get("path", "") or "/"
    app_prefix = normalized_app_prefix()

    for prefix in (root_path.rstrip("/"), app_prefix):
        if prefix and path == prefix:
            path = "/"
            break
        if prefix and path.startswith(f"{prefix}/"):
            path = path[len(prefix):]
            break

    if not path.startswith("/"):
        path = f"/{path}"

    return root_path, app_prefix, path


def wait_for_go_health(attempts: int = 10, delay_seconds: float = 0.5) -> bool:
    health_url = f"{GO_URL}/api/health"
    for attempt in range(1, attempts + 1):
        try:
            response = httpx.get(health_url, timeout=2.0)
            log(f"health probe {attempt}/{attempts}: {response.status_code} {response.text[:120]!r}")
            if response.is_success:
                return True
        except Exception as exc:
            log(f"health probe {attempt}/{attempts} failed: {exc}")
        time.sleep(delay_seconds)
    return False


def start_go_api() -> None:
    global GO_PROCESS, GO_READY, GO_START_ERROR

    log(f"expected Go binary at {GO_BIN}")
    if not GO_BIN.exists():
        GO_START_ERROR = f"Go binary not found at {GO_BIN}"
        log(GO_START_ERROR)
        return

    try:
        GO_BIN.chmod(GO_BIN.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)
        cmd = [
            str(GO_BIN),
            "--port",
            str(GO_PORT),
            "--data-dir",
            str(ROOT / "data"),
            "--www",
            str(ROOT / "www"),
        ]
        log(f"starting Go API: {' '.join(cmd)}")
        GO_PROCESS = subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        log(f"Go API spawned with pid={GO_PROCESS.pid}")
        GO_READY = wait_for_go_health()
        if GO_READY:
            GO_START_ERROR = None
            log("Go API health check passed")
        else:
            GO_START_ERROR = "Go API failed health check during startup"
            log(GO_START_ERROR)
    except Exception as exc:
        GO_START_ERROR = f"Go API start failed: {exc}"
        log(GO_START_ERROR)


start_go_api()


class GoProxyASGIMiddleware:
    def __init__(self, app):
        self.app = app
        self.request_log_count = 0

    async def __call__(self, scope, receive, send):
        global GO_READY, GO_START_ERROR

        if scope["type"] != "http":
            return await self.app(scope, receive, send)

        root_path, app_prefix, path = upstream_path_for_scope(scope)
        method = scope["method"]
        qs = scope.get("query_string", b"")
        query = qs.decode() if qs else ""
        url = f"{GO_URL}{path}"
        if query:
            url = f"{url}?{query}"

        if self.request_log_count < REQUEST_LOG_LIMIT:
            self.request_log_count += 1
            log(
                "proxy probe #{}: method={} root_path={!r} app_prefix={!r} path={!r} upstream={!r}{}".format(
                    self.request_log_count,
                    method,
                    root_path,
                    app_prefix,
                    scope.get("path", ""),
                    path,
                    f"?{query}" if query else "",
                )
            )

        body = b""
        more_body = True
        while more_body:
            message = await receive()
            if message["type"] == "http.request":
                body += message.get("body", b"")
                more_body = message.get("more_body", False)

        try:
            async with httpx.AsyncClient() as client:
                headers = {}
                for key, value in scope.get("headers", []):
                    header_name = key.decode().lower()
                    if header_name in ("content-type", "accept"):
                        headers[header_name] = value.decode()

                response = await client.request(
                    method,
                    url,
                    content=body or None,
                    headers=headers,
                    timeout=30.0,
                )

            GO_READY = True
            GO_START_ERROR = None
            await send(
                {
                    "type": "http.response.start",
                    "status": response.status_code,
                    "headers": [
                        [b"content-type", response.headers.get("content-type", "application/octet-stream").encode()]
                    ],
                }
            )
            await send({"type": "http.response.body", "body": response.content})
        except Exception as exc:
            GO_READY = False
            if GO_START_ERROR is None:
                GO_START_ERROR = f"Go API unavailable during request: {exc}"
            log(f"proxy error for {method} {path}: {exc}")
            message = (
                f"mellon Go proxy unavailable.\n"
                f"path: {path}\n"
                f"upstream: {GO_URL}\n"
                f"detail: {GO_START_ERROR}\n"
                f"hint: if shinyapps.io logs never show 'proxy probe', the platform is bypassing app.starlette_app.\n"
            ).encode()
            await send(
                {
                    "type": "http.response.start",
                    "status": 503,
                    "headers": [[b"content-type", b"text/plain; charset=utf-8"]],
                }
            )
            await send({"type": "http.response.body", "body": message})


app = shiny.App(shiny.ui.tags.div(), None)
app.starlette_app = GoProxyASGIMiddleware(app.starlette_app)

if __name__ == "__main__":
    shiny.run_app(app, host="0.0.0.0", port=8000)
