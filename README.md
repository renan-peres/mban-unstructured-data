# Business Analysis with Unstructured Data
## Spring 2026

## Shinylive + Netlify (Airbnb App)

A Shinylive-ready app is available in `apps/airbnb-shinylive`.

Build commands from repository root:

```bash
Rscript apps/airbnb-shinylive/scripts/prepare_shinylive_data.R
Rscript apps/airbnb-shinylive/scripts/export_shinylive.R
```

This exports static files to `site/airbnb-shinylive`.

Netlify is configured by `netlify.toml` to publish `site` and redirect `/` to `/airbnb-shinylive/`.
