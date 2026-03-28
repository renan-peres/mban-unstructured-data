suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(textdata)
  library(rappdirs)
})

options(menu.graphics = FALSE)

text_cols <- c(
  "summary", "space", "description", "neighborhood_overview",
  "notes", "transit", "access", "interaction", "house_rules"
)

# Only keep columns required by apps/airbnb-shinylive/app.R.
base_keep_cols <- c(
  "listing_url",
  "name",
  "property_type",
  "room_type",
  "price",
  "images.picture_url",
  "address.street",
  "address.country",
  "address.market",
  "address.location.coordinates",
  "host.host_is_superhost",
  "review_scores.review_scores_rating"
)

to_scalar_character <- function(x) {
  if (is.list(x)) {
    return(vapply(x, function(el) paste(as.character(el), collapse = " "), character(1)))
  }
  as.character(x)
}

parse_coordinates <- function(x, index) {
  values <- as.character(x)
  parts <- strsplit(gsub("\\[|\\]", "", values), ",")
  vapply(
    parts,
    function(value) {
      if (length(value) < index) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(trimws(value[index])))
    },
    numeric(1)
  )
}

# Navigate to repo root (3 levels up from scripts/)
repo_root <- normalizePath("../../..", winslash = "/")
source_candidates <- c(
  file.path(repo_root, "#A1 - AirBnb Text Mining & NLP", "data", "airbnb_all.parquet"),
  file.path(repo_root, "#A1 - AirBnb Text Mining & NLP", "airbnb-text-mining-nlp", "data", "airbnb_all.parquet")
)
source_parquet <- source_candidates[file.exists(source_candidates)][1]
out_dir <- file.path(repo_root, "apps", "airbnb-shinylive", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (is.na(source_parquet) || !file.exists(source_parquet)) {
  stop(
    "Could not find source parquet file. Checked:\n",
    paste0("- ", source_candidates, collapse = "\n"),
    "\nWorking directory: ", getwd()
  )
}

# You can override these defaults via env vars during CI runs.
max_rows <- as.integer(Sys.getenv("SHINYLIVE_MAX_ROWS", unset = "1500"))
max_text_chars <- as.integer(Sys.getenv("SHINYLIVE_MAX_TEXT_CHARS", unset = "2500"))

raw_data <- arrow::read_parquet(source_parquet, as_data_frame = TRUE)

if (!is.na(max_rows) && max_rows > 0 && nrow(raw_data) > max_rows) {
  set.seed(42)
  raw_data <- dplyr::sample_n(raw_data, max_rows)
}

airbnb_data <- raw_data |>
  mutate(row_id = row_number()) |>
  mutate(across(any_of(text_cols), to_scalar_character)) |>
  mutate(across(any_of(base_keep_cols), to_scalar_character)) |>
  mutate(across(any_of(text_cols), ~ replace_na(.x, "")))

# Build row-wise text safely; avoid tidyr::unite on nested/list-like columns.
text_df <- airbnb_data[, intersect(text_cols, names(airbnb_data)), drop = FALSE]
airbnb_data$text <- do.call(paste, c(text_df, sep = " "))
airbnb_data$text <- gsub("\\s+", " ", trimws(airbnb_data$text))
airbnb_data$text <- substr(airbnb_data$text, 1, max_text_chars)

airbnb_data$price <- suppressWarnings(as.numeric(airbnb_data$price))
airbnb_data$longitude <- parse_coordinates(airbnb_data[["address.location.coordinates"]], 1)
airbnb_data$latitude <- parse_coordinates(airbnb_data[["address.location.coordinates"]], 2)

# Keep only fields used by the app plus row_id/text/coordinates.
final_keep <- c(
  "row_id",
  base_keep_cols,
  "text",
  "longitude",
  "latitude"
)
airbnb_data <- airbnb_data[, intersect(final_keep, names(airbnb_data)), drop = FALSE]

stop_words_vec <- tidytext::stop_words$word

all_words <- airbnb_data |>
  select(all_of(c("row_id", "text"))) |>
  unnest_tokens("word", "text")

all_tokens <- all_words |>
  filter(!.data$word %in% stop_words_vec)

all_bigrams <- airbnb_data |>
  select(all_of(c("row_id", "text"))) |>
  unnest_tokens("bigram", "text", token = "ngrams", n = 2) |>
  separate("bigram", c("word1", "word2"), sep = " ", fill = "right") |>
  filter(!.data$word1 %in% stop_words_vec, !.data$word2 %in% stop_words_vec) |>
  unite("bigram", c("word1", "word2"), sep = " ")

# Ensure sentiment lexicons are cached without interactive prompts.
ensure_textdata_lexicon <- function(name, rds_name) {
  cache_root <- rappdirs::user_cache_dir("textdata")
  folder_path <- file.path(cache_root, name)
  rds_path <- file.path(folder_path, rds_name)

  if (file.exists(rds_path)) {
    return(invisible(rds_path))
  }

  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  textdata:::download_functions[[name]](folder_path)
  textdata:::process_functions[[name]](folder_path, rds_path)
  invisible(rds_path)
}

ensure_textdata_lexicon("afinn", "afinn_111.rds")
ensure_textdata_lexicon("bing", "bing.rds")
ensure_textdata_lexicon("nrc", "NRCWordEmotion.rds")

afinn_lex <- get_sentiments("afinn")
bing_lex <- get_sentiments("bing")
nrc_lex <- get_sentiments("nrc") |>
  filter(.data$sentiment %in% c("positive", "negative"))

all_afinn <- inner_join(all_words, afinn_lex, by = "word")
all_bing <- inner_join(all_words, bing_lex, by = "word")
all_nrc <- inner_join(all_words, nrc_lex, by = "word", relationship = "many-to-many")

saveRDS(airbnb_data, file.path(out_dir, "airbnb_data.rds"), compress = "xz")
saveRDS(all_words, file.path(out_dir, "all_words.rds"), compress = "xz")
saveRDS(all_tokens, file.path(out_dir, "all_tokens.rds"), compress = "xz")
saveRDS(all_bigrams, file.path(out_dir, "all_bigrams.rds"), compress = "xz")
saveRDS(all_afinn, file.path(out_dir, "all_afinn.rds"), compress = "xz")
saveRDS(all_bing, file.path(out_dir, "all_bing.rds"), compress = "xz")
saveRDS(all_nrc, file.path(out_dir, "all_nrc.rds"), compress = "xz")

sizes <- file.info(file.path(out_dir, c(
  "airbnb_data.rds",
  "all_words.rds",
  "all_tokens.rds",
  "all_bigrams.rds",
  "all_afinn.rds",
  "all_bing.rds",
  "all_nrc.rds"
)))$size
cat("Prepared shinylive data files in:", out_dir, "\n")
cat("Total data size (MB):", round(sum(sizes, na.rm = TRUE) / 1024^2, 2), "\n")
