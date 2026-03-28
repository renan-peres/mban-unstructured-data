# Airbnb Shinylive Build

This folder contains a Shinylive-compatible version of the Airbnb Shiny app.

## Files

- `app.R`: Shinylive app code.
- `data/*.rds`: Precomputed datasets used by the app in the browser.
- `scripts/prepare_shinylive_data.R`: Builds the precomputed RDS files from the original parquet source.
- `scripts/export_shinylive.R`: Exports a static Shinylive site to `site/airbnb-shinylive`.

## Build Steps

From the repository root:

```bash
Rscript apps/airbnb-shinylive/scripts/prepare_shinylive_data.R
Rscript apps/airbnb-shinylive/scripts/export_shinylive.R
```

The exported site is generated in:

- `site/airbnb-shinylive`

`netlify.toml` is configured so Netlify can publish the `site` folder and redirect `/` to this app.

## Required R packages (build machine)

- `shinylive`
- `arrow`
- `tidytext`
- `textdata`
- `rappdirs`

If `shinylive` fails to install on Ubuntu due `archive` package compilation, install system dependency:

```bash
sudo apt-get update && sudo apt-get install -y libarchive-dev
```
