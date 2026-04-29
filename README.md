# Hematological Drug Assistant

血液科用药助手 — React SPA + Go API + Python Shiny proxy.

## Architecture

| Layer | Tech | Purpose |
|-------|------|---------|
| Frontend | React 18 + TypeScript + Ant Design | UI, i18n, 5 themes |
| Backend | Go (mellon-api) | Drug regimen dosing, scoring, side effects |
| Proxy | Python Shiny (app.py) | shinyapps.io entry, routes to Go |
| Data | JSON | 63 regimens, 57 drugs, scoring reference tables |

## Local Development

```bash
# Start Go API
./bin/mellon-api --port 3939 --data-dir data --www www

# Dev frontend with hot reload
cd ui && npm run dev
```

Open http://127.0.0.1:3939

## Deployment

```bash
bash scripts/deploy_shinyapps.sh
```

GitHub Actions auto-deploys on push to `master`.

## Secrets

Set in GitHub repo → Settings → Secrets → Actions:

| Secret | Value |
|--------|-------|
| `RSCONNECT_ACCOUNT` | shinyapps.io account name |
| `RSCONNECT_TOKEN` | shinyapps.io token |
| `RSCONNECT_SECRET` | shinyapps.io secret |
