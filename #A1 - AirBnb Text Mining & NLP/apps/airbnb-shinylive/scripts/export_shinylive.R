suppressPackageStartupMessages({
  library(shinylive)
})

repo_root <- normalizePath(file.path(getwd(), "..", "..", ".."), mustWork = TRUE)
app_dir <- file.path(repo_root, "apps", "airbnb-shinylive")
site_dir <- file.path(repo_root, "site")
subdir_name <- "airbnb-shinylive"

if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("Could not find app.R in: ", app_dir)
}

# Clean prior exported app subfolder to avoid stale assets.
unlink(file.path(site_dir, subdir_name), recursive = TRUE, force = TRUE)
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

shinylive::export(
  appdir = app_dir,
  destdir = site_dir,
  subdir = subdir_name
)

cat("Shinylive app exported to:", file.path(site_dir, subdir_name), "\n")
