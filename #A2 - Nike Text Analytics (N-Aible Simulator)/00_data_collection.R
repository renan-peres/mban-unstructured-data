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

# Keep paths stable across OSes (Windows/macOS/Linux) and launch modes.
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", cmd_args[grep(file_arg, cmd_args)])
  
  if (length(script_path) > 0 && nzchar(script_path[1])) {
    return(dirname(normalizePath(script_path[1], winslash = "/", mustWork = FALSE)))
  }
  
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    active_path <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path, winslash = "/", mustWork = FALSE)))
    }
  }
  
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

setwd(get_script_dir())

# Create data/ folder if it doesn't exist
if (!dir.exists("data")) dir.create("data", recursive = TRUE)


# =============================================================================
# 1. REDDIT EXTRACTION
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("  REDDIT DATA EXTRACTION\n")
cat(strrep("=", 60), "\n\n")

reddit_request_pause <- 1.5
reddit_rate_limit_wait <- 60
reddit_max_attempts <- 5

extract_reddit_brand <- function(keywords, subreddits, brand_name,
                                 max_threads_per_sub = 10) {
  
  is_rate_limit_error <- function(message_text) {
    if (is.null(message_text) || length(message_text) == 0 || is.na(message_text)) {
      return(FALSE)
    }
    
    stringr::str_detect(
      stringr::str_to_lower(message_text),
      paste(
        c(
          "rate limit",
          "too many requests",
          "http 429",
          "status code 429",
          "max extraction",
          "try again later",
          "cannot open the connection",
          "connection reset",
          "timed out",
          "timeout was reached"
        ),
        collapse = "|"
      )
    )
  }
  
  wait_for_reddit_limit <- function(attempt, retry_after = NULL, context = "Reddit request") {
    retry_value <- retry_after
    if (length(retry_value) == 0 || is.null(retry_value)) {
      retry_value <- NA_character_
    } else {
      retry_value <- retry_value[[1]]
    }
    
    wait_seconds <- suppressWarnings(as.numeric(retry_value))
    
    if (length(wait_seconds) == 0 || is.na(wait_seconds[[1]]) ||
        !is.finite(wait_seconds[[1]]) || wait_seconds[[1]] <= 0) {
      wait_seconds <- reddit_rate_limit_wait * attempt
    } else {
      wait_seconds <- wait_seconds[[1]]
    }
    
    wait_seconds <- min(wait_seconds, 10 * 60)
    cat(sprintf("    [Wait] %s throttled. Sleeping for %.0f seconds before retrying...\n",
                context, wait_seconds))
    Sys.sleep(wait_seconds)
  }
  
  call_with_reddit_backoff <- function(expr, context, max_attempts = reddit_max_attempts) {
    for (attempt in seq_len(max_attempts)) {
      result <- tryCatch(expr(), error = identity)
      
      if (!inherits(result, "error")) {
        return(result)
      }
      
      if (attempt < max_attempts && is_rate_limit_error(conditionMessage(result))) {
        wait_for_reddit_limit(attempt = attempt, context = context)
      } else {
        if (attempt < max_attempts) {
          Sys.sleep(reddit_request_pause * attempt)
        } else {
          message(sprintf("    [Skip] %s failed after %d attempts: %s",
                          context, max_attempts, conditionMessage(result)))
        }
      }
    }
    
    NULL
  }
  
  safe_get_json <- function(url, max_attempts = 4, pause_seconds = 1.5) {
    ua <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    
    for (attempt in seq_len(max_attempts)) {
      resp <- tryCatch(
        httr::GET(
          url,
          httr::add_headers(
            "User-Agent" = ua,
            "Accept" = "application/json"
          ),
          httr::timeout(20)
        ),
        error = function(e) NULL
      )
      
      if (!is.null(resp) && httr::status_code(resp) == 200) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(
          jsonlite::fromJSON(txt),
          error = function(e) NULL
        )
        if (!is.null(parsed)) return(parsed)
      }
      
      if (!is.null(resp) && httr::status_code(resp) == 429 && attempt < max_attempts) {
        retry_after <- httr::headers(resp)[["retry-after"]]
        if (is.null(retry_after)) {
          retry_after <- httr::headers(resp)[["x-ratelimit-reset"]]
        }
        wait_for_reddit_limit(
          attempt = attempt,
          retry_after = retry_after,
          context = "Reddit JSON API"
        )
        next
      }
      
      if (attempt < max_attempts) {
        wait <- pause_seconds * attempt
        Sys.sleep(wait)
      }
    }
    
    NULL
  }
  
  reddit_search_urls_fallback <- function(keyword, subreddit, limit = 100) {
    query <- utils::URLencode(keyword, reserved = TRUE)
    search_url <- paste0(
      "https://www.reddit.com/r/", subreddit,
      "/search.json?restrict_sr=on&q=", query,
      "&sort=top&t=year&limit=", limit,
      "&raw_json=1"
    )
    
    search_json <- safe_get_json(search_url)
    if (is.null(search_json) || is.null(search_json$data$children) ||
        length(search_json$data$children) == 0) {
      return(tibble::tibble(url = character()))
    }
    
    posts <- search_json$data$children$data
    posts_df <- tryCatch(tibble::as_tibble(posts), error = function(e) tibble::tibble())
    if (nrow(posts_df) == 0 || !"permalink" %in% names(posts_df)) {
      return(tibble::tibble(url = character()))
    }
    
    if (!"url_overridden_by_dest" %in% names(posts_df)) {
      posts_df$url_overridden_by_dest <- NA_character_
    }
    
    threads <- posts_df %>%
      dplyr::transmute(
        url = dplyr::if_else(
          !is.na(.data$url_overridden_by_dest) & stringr::str_detect(.data$url_overridden_by_dest, "^https?://"),
          .data$url_overridden_by_dest,
          paste0("https://www.reddit.com", .data$permalink)
        )
      ) %>%
      dplyr::filter(stringr::str_detect(url, "/comments/")) %>%
      dplyr::distinct(url)
    
    threads
  }
  
  reddit_thread_comments_fallback <- function(thread_url, subreddit, brand_name) {
    clean_url <- stringr::str_remove(thread_url, "\\?.*$")
    comments_url <- paste0(clean_url, ".json?limit=500&raw_json=1")
    
    thread_json <- safe_get_json(comments_url, max_attempts = 3, pause_seconds = 2)
    if (is.null(thread_json) || length(thread_json) < 2 ||
        is.null(thread_json[[2]]$data$children) ||
        length(thread_json[[2]]$data$children) == 0) {
      return(tibble::tibble())
    }
    
    comments_df <- tryCatch(
      tibble::as_tibble(thread_json[[2]]$data$children$data),
      error = function(e) tibble::tibble()
    )
    
    if (nrow(comments_df) == 0 || !"body" %in% names(comments_df)) {
      return(tibble::tibble())
    }
    
    score_col <- dplyr::case_when(
      "score" %in% names(comments_df) ~ "score",
      "ups"   %in% names(comments_df) ~ "ups",
      TRUE                              ~ NA_character_
    )
    
    comments_df %>%
      dplyr::transmute(
        text = body,
        rating = if (!is.na(score_col)) as.numeric(.data[[score_col]]) else NA_real_,
        source = paste0("Reddit (r/", subreddit, ")"),
        brand = brand_name
      ) %>%
      dplyr::filter(!is.na(text), nchar(stringr::str_trim(text)) > 10)
  }
  
  all_comments <- list()
  
  for (sub in unique(subreddits)) {
    for (kw in keywords) {
      cat(sprintf("  Searching r/%s for '%s'...\n", sub, kw))
      
      urls_df <- call_with_reddit_backoff(
        expr = function() find_thread_urls(
          keywords  = kw,
          subreddit = sub,
          sort_by   = "top",
          period    = "year"
        ),
        context = sprintf("find_thread_urls(r/%s, '%s')", sub, kw)
      )
      
      if (is.null(urls_df) || !is.data.frame(urls_df) || nrow(urls_df) == 0) {
        urls_df <- reddit_search_urls_fallback(keyword = kw, subreddit = sub)
      }
      
      if (is.null(urls_df) || !is.data.frame(urls_df) || nrow(urls_df) == 0) next
      
      n_threads   <- min(max_threads_per_sub, nrow(urls_df))
      thread_urls <- urls_df$url[1:n_threads]
      cat(sprintf("    Found %d threads, fetching comments from top %d...\n",
                  nrow(urls_df), n_threads))
      
      clean <- tibble::tibble()
      
      # First try RedditExtractoR parser (often works even when direct JSON is blocked).
      thread_data <- call_with_reddit_backoff(
        expr = function() get_thread_content(thread_urls),
        context = sprintf("get_thread_content(r/%s, '%s')", sub, kw)
      )
      
      if (!is.null(thread_data) &&
          !is.null(thread_data$comments) &&
          is.data.frame(thread_data$comments) &&
          nrow(thread_data$comments) > 0) {
        
        comments_df <- thread_data$comments
        
        text_col <- dplyr::case_when(
          "comment" %in% names(comments_df) ~ "comment",
          "body"    %in% names(comments_df) ~ "body",
          TRUE                                ~ NA_character_
        )
        
        score_col <- dplyr::case_when(
          "comment_score" %in% names(comments_df) ~ "comment_score",
          "score"         %in% names(comments_df) ~ "score",
          "ups"           %in% names(comments_df) ~ "ups",
          TRUE                                      ~ NA_character_
        )
        
        if (!is.na(text_col)) {
          clean <- comments_df %>%
            dplyr::transmute(
              text = .data[[text_col]],
              rating = if (!is.na(score_col)) as.numeric(.data[[score_col]]) else NA_real_,
              source = paste0("Reddit (r/", sub, ")"),
              brand = brand_name
            ) %>%
            dplyr::filter(!is.na(text), nchar(stringr::str_trim(text)) > 10)
        }
      }
      
      # If primary parser returns nothing, fall back to direct JSON thread calls.
      if (nrow(clean) == 0) {
        clean <- dplyr::bind_rows(lapply(thread_urls, function(u) {
          tryCatch(
            reddit_thread_comments_fallback(
              thread_url = u,
              subreddit = sub,
              brand_name = brand_name
            ),
            error = function(e) tibble::tibble()
          )
        }))
      }
      
      if (nrow(clean) > 0) {
        all_comments[[paste(sub, kw)]] <- clean
        cat(sprintf("    Collected %d comments\n", nrow(clean)))
      } else {
        cat("    [Skip] No comments collected from selected threads\n")
      }
      
      Sys.sleep(reddit_request_pause)
    }
  }
  
  dplyr::bind_rows(all_comments)
}

# Shared sports-related channels plus brand-specific communities.
common_sport_subreddits <- c(
  "Sneakers",
  "RunningShoeGeeks",
  "Running",
  "BasketballShoes",
  "trailrunning"
)

# ── Nike ──
cat("\n[Nike]\n")
nike_reddit <- extract_reddit_brand(
  keywords   = c("Nike"),
  subreddits = c("Nike", common_sport_subreddits),
  brand_name = "Nike",
  max_threads_per_sub = 8
)

# ── Adidas ──
cat("\n[Adidas]\n")
adidas_reddit <- extract_reddit_brand(
  keywords   = c("Adidas"),
  subreddits = c("adidas", common_sport_subreddits),
  brand_name = "Adidas",
  max_threads_per_sub = 8
)

# ── Under Armour ──
cat("\n[Under Armour]\n")
ua_reddit <- extract_reddit_brand(
  keywords   = c("Under Armour"),
  subreddits = c("UnderArmour", common_sport_subreddits),
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