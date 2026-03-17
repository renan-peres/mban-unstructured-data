# =============================================================================
# 00_data_collection.R
# Nike Text Analytics -- Data Acquisition Script
# Pulls data from Reddit and YouTube APIs, saves to data/ as CSVs
#
# Run this ONCE before knitting the RMarkdown analysis file.
# Output: data/reddit_comments.csv, data/youtube_comments.csv
# =============================================================================

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_pkgs <- c("tidyverse", "RedditExtractoR", "httr", "jsonlite")

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                 FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(RedditExtractoR)
library(httr)
library(jsonlite)

set.seed(42)

# Create data/ folder if it doesn't exist
if (!dir.exists("data")) dir.create("data", recursive = TRUE)


# =============================================================================
# 1. REDDIT EXTRACTION
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("  REDDIT DATA EXTRACTION\n")
cat(strrep("=", 60), "\n\n")

extract_reddit_brand <- function(keywords, subreddits, brand_name,
                                 max_threads_per_sub = 10) {
  
  all_comments <- list()
  
  for (sub in subreddits) {
    for (kw in keywords) {
      cat(sprintf("  Searching r/%s for '%s'...\n", sub, kw))
      
      urls_df <- tryCatch(
        find_thread_urls(
          keywords  = kw,
          subreddit = sub,
          sort_by   = "top",
          period    = "year"
        ),
        error = function(e) {
          message(sprintf("    [Skip] No results for '%s' in r/%s: %s",
                          kw, sub, e$message))
          NULL
        }
      )
      
      if (is.null(urls_df) || !is.data.frame(urls_df) || nrow(urls_df) == 0) next
      
      n_threads   <- min(max_threads_per_sub, nrow(urls_df))
      thread_urls <- urls_df$url[1:n_threads]
      cat(sprintf("    Found %d threads, fetching comments from top %d...\n",
                  nrow(urls_df), n_threads))
      
      thread_data <- tryCatch(
        get_thread_content(thread_urls),
        error = function(e) {
          message(sprintf("    [Skip] Could not fetch comments: %s", e$message))
          NULL
        }
      )
      
      if (!is.null(thread_data$comments) && nrow(thread_data$comments) > 0) {
        
        comments_df <- thread_data$comments
        
        # Choose an available score column
        score_col <- dplyr::case_when(
          "comment_score" %in% names(comments_df) ~ "comment_score",
          "score"         %in% names(comments_df) ~ "score",
          TRUE                                   ~ NA_character_
        )
        
        clean <- comments_df %>%
          dplyr::transmute(
            text   = comment,
            rating = if (!is.na(score_col)) as.numeric(.data[[score_col]]) else NA_real_,
            source = paste0("Reddit (r/", sub, ")"),
            brand  = brand_name
          ) %>%
          dplyr::filter(!is.na(text), nchar(stringr::str_trim(text)) > 10)
        
        all_comments[[paste(sub, kw)]] <- clean
        cat(sprintf("    Collected %d comments\n", nrow(clean)))
      }
      
      Sys.sleep(1)
    }
  }
  
  dplyr::bind_rows(all_comments)
}

# ── Nike ──
cat("\n[Nike]\n")
nike_reddit <- extract_reddit_brand(
  keywords   = c("Nike", "Nike shoes", "Air Max", "Pegasus", "Jordan"),
  subreddits = c("Sneakers", "Nike", "RunningShoeGeeks"),
  brand_name = "Nike",
  max_threads_per_sub = 8
)

# ── Adidas ──
cat("\n[Adidas]\n")
adidas_reddit <- extract_reddit_brand(
  keywords   = c("Adidas", "Adidas shoes", "Ultraboost", "Samba"),
  subreddits = c("Sneakers", "Adidas", "RunningShoeGeeks"),
  brand_name = "Adidas",
  max_threads_per_sub = 8
)

# ── Under Armour ──
cat("\n[Under Armour]\n")
ua_reddit <- extract_reddit_brand(
  keywords   = c("Under Armour", "UA shoes", "HOVR"),
  subreddits = c("Sneakers", "UnderArmour", "RunningShoeGeeks"),
  brand_name = "Under Armour",
  max_threads_per_sub = 8
)

# Combine and save
all_reddit <- bind_rows(nike_reddit, adidas_reddit, ua_reddit) %>%
  mutate(doc_id = paste0("RED_", row_number()))

write_csv(all_reddit, "data/reddit_comments.csv")
cat(sprintf("\nReddit DONE: %d comments saved to data/reddit_comments.csv\n", nrow(all_reddit)))
cat(sprintf("  Nike: %d | Adidas: %d | Under Armour: %d\n",
            sum(all_reddit$brand == "Nike"),
            sum(all_reddit$brand == "Adidas"),
            sum(all_reddit$brand == "Under Armour")))


# =============================================================================
# 2. YOUTUBE EXTRACTION
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("  YOUTUBE DATA EXTRACTION\n")
cat(strrep("=", 60), "\n\n")

yt_api_key <- "AIzaSyBj8m1dl4muRve9F_8ph8uFOgzQpM3Zs8w"

# Helper: search YouTube for videos matching a query
search_youtube_videos <- function(query, api_key, max_results = 10) {
  url <- paste0(
    "https://www.googleapis.com/youtube/v3/search?",
    "part=snippet",
    "&q=", URLencode(query),
    "&type=video",
    "&maxResults=", max_results,
    "&key=", api_key
  )
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if (is.null(data$items) || length(data$items) == 0) {
    return(tibble(video_id = character(), title = character()))
  }
  
  tibble(
    video_id = data$items$id$videoId,
    title    = data$items$snippet$title
  ) %>%
    filter(!is.na(video_id))
}

# Helper: pull top-level comments from a single video
get_youtube_comments <- function(video_id, api_key, max_results = 100) {
  url <- paste0(
    "https://www.googleapis.com/youtube/v3/commentThreads?",
    "part=snippet",
    "&videoId=", video_id,
    "&maxResults=", max_results,
    "&key=", api_key
  )
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if (is.null(data$items) || length(data$items) == 0) return(NULL)
  
  tibble(
    video_id = video_id,
    comment  = data$items$snippet$topLevelComment$snippet$textDisplay,
    likes    = data$items$snippet$topLevelComment$snippet$likeCount
  )
}

# ── Search across Nike-focused queries ──
yt_queries <- c(
  "Nike shoe review 2025",
  "Nike shoe review 2024",
  "Nike vs Adidas",
  "Nike sustainability",
  "Nike pricing worth it",
  "Adidas shoe review 2025",
  "Under Armour shoe review 2025"
)

cat("Searching YouTube for videos...\n")
all_yt_videos <- bind_rows(lapply(yt_queries, function(q) {
  cat(sprintf("  Query: '%s'\n", q))
  tryCatch(
    search_youtube_videos(q, yt_api_key, max_results = 10),
    error = function(e) {
      message(sprintf("    [Skip] Search failed: %s", e$message))
      tibble(video_id = character(), title = character())
    }
  )
})) %>%
  distinct(video_id, .keep_all = TRUE)

cat(sprintf("Unique videos found: %d\n\n", nrow(all_yt_videos)))

# ── Pull comments from all discovered videos ──
cat("Extracting comments from videos...\n")
all_yt_comments_raw <- bind_rows(lapply(seq_len(nrow(all_yt_videos)), function(i) {
  vid    <- all_yt_videos$video_id[i]
  vtitle <- all_yt_videos$title[i]
  cat(sprintf("  [%d/%d] %s\n", i, nrow(all_yt_videos), vtitle))
  Sys.sleep(0.5)
  tryCatch(get_youtube_comments(vid, yt_api_key), error = function(e) NULL)
}))

cat(sprintf("\nRaw YouTube comments collected: %d\n", nrow(all_yt_comments_raw)))

# ── Clean and standardize ──
all_youtube <- all_yt_comments_raw %>%
  left_join(all_yt_videos %>% select(video_id, title), by = "video_id") %>%
  filter(!is.na(comment), nchar(comment) > 15) %>%
  mutate(
    brand = case_when(
      str_detect(str_to_lower(title), "under\\s*armou?r|\\bua\\b") ~ "Under Armour",
      str_detect(str_to_lower(title), "\\badidas\\b")              ~ "Adidas",
      TRUE                                                         ~ "Nike"
    ),
    text   = comment,
    rating = as.numeric(likes),
    source = "YouTube"
  ) %>%
  select(text, rating, source, brand) %>%
  mutate(doc_id = paste0("YT_", row_number()))

write_csv(all_youtube, "data/youtube_comments.csv")
cat(sprintf("\nYouTube DONE: %d comments saved to data/youtube_comments.csv\n", nrow(all_youtube)))
cat(sprintf("  Nike: %d | Adidas: %d | Under Armour: %d\n",
            sum(all_youtube$brand == "Nike"),
            sum(all_youtube$brand == "Adidas"),
            sum(all_youtube$brand == "Under Armour")))


# =============================================================================
# 3. SUMMARY
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("  DATA COLLECTION COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(sprintf("  Reddit:  %d comments  -> data/reddit_comments.csv\n", nrow(all_reddit)))
cat(sprintf("  YouTube: %d comments  -> data/youtube_comments.csv\n", nrow(all_youtube)))
cat(sprintf("  Total:   %d comments\n", nrow(all_reddit) + nrow(all_youtube)))
cat("\nYou can now knit A2_Nike_Text_Analytics_Classification_Improved.Rmd\n")