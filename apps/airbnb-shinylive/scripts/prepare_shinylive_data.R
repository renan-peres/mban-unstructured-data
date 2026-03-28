suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(textdata)
  library(rappdirs)
})

text_cols <- c(
  "summary", "space", "description", "neighborhood_overview",
  "notes", "transit", "access", "interaction", "house_rules"
)

parse_coordinates <- function(x, index) {
  parts <- strsplit(gsub("\\[|\\]", "", x), ",")
  vapply(
    parts,
    function(value) {
      if (length(value) < index) return(NA_real_)
      suppressWarnings(as.numeric(trimws(value[index])))
    },
    numeric(1)
  )
}

# Navigate to repo root (3 levels up from scripts/)
repo_root <- normalizePath("../../..", winslash = "/")
source_parquet <- file.path(repo_root, "#A1 - AirBnb Text Mining & NLP", "data", "airbnb_all.parquet")
out_dir <- file.path(repo_root, "apps", "airbnb-shinylive", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(source_parquet)) {
  stop("Could not find source parquet file at: ", source_parquet, "\nWorking directory: ", getwd())
}

airbnb_data <- arrow::read_parquet(source_parquet, as_data_frame = TRUE) |>
  mutate(row_id = row_number()) |>
  mutate(across(any_of(text_cols), ~ replace_na(as.character(.x), ""))) |>
  unite("text", any_of(text_cols), sep = " ", remove = FALSE, na.rm = TRUE) |>
  mutate(
    text = trimws(text),
    longitude = parse_coordinates(.data[["address.location.coordinates"]], 1),
    latitude = parse_coordinates(.data[["address.location.coordinates"]], 2)
  )

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
ensure_textdata_lexicon("nrc", "nrc.rds")

afinn_lex <- get_sentiments("afinn")
bing_lex <- get_sentiments("bing")
nrc_lex <- get_sentiments("nrc") |>
  filter(.data$sentiment %in% c("positive", "negative"))

all_afinn <- inner_join(all_words, afinn_lex, by = "word")
all_bing <- inner_join(all_words, bing_lex, by = "word")
all_nrc <- inner_join(all_words, nrc_lex, by = "word", relationship = "many-to-many")

saveRDS(airbnb_data, file.path(out_dir, "airbnb_data.rds"))
saveRDS(all_words, file.path(out_dir, "all_words.rds"))
saveRDS(all_tokens, file.path(out_dir, "all_tokens.rds"))
saveRDS(all_bigrams, file.path(out_dir, "all_bigrams.rds"))
saveRDS(all_afinn, file.path(out_dir, "all_afinn.rds"))
saveRDS(all_bing, file.path(out_dir, "all_bing.rds"))
saveRDS(all_nrc, file.path(out_dir, "all_nrc.rds"))

cat("Prepared shinylive data files in:", out_dir, "\n")
