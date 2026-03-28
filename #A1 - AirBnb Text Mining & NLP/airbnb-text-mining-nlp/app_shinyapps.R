## Install Packages
# options(download.file.method = "libcurl")
# install.packages("leaflet", repos = "https://cloud.r-project.org")

library(shiny)
library(arrow)
library(curl)
library(dplyr)
library(tidyr)
library(tidytext)
library(textdata)
library(ggplot2)
library(DT)
library(plotly)
library(scales)

airbnb_url <- paste0(
  "https://github.com/renan-peres/mban-unstructured-data/raw/",
  "refs/heads/main/%23A1%20-%20AirBnb%20Text%20Mining%20%26%20NLP/",
  "data/airbnb_all.parquet"
)

airbnb_local_path <- file.path("data", "airbnb_all.parquet")

text_cols <- c(
  "summary", "space", "description", "neighborhood_overview",
  "notes", "transit", "access", "interaction", "house_rules"
)

required_cols <- unique(c(
  text_cols,
  "address.location.coordinates",
  "address.country",
  "address.market",
  "room_type",
  "price",
  "host.host_is_superhost",
  "review_scores.review_scores_rating",
  "listing_url",
  "images.picture_url",
  "name",
  "address.street",
  "property_type"
))

read_airbnb_parquet <- function(source) {
  arrow::read_parquet(
    source,
    col_select = any_of(required_cols),
    as_data_frame = TRUE
  )
}

load_airbnb <- function() {
  if (file.exists(airbnb_local_path)) {
    message("Loading Airbnb data from local parquet: ", airbnb_local_path)
    return(read_airbnb_parquet(airbnb_local_path))
  }

  message("Local parquet not found. Loading Airbnb data from GitHub.")
  raw_parquet <- curl::curl_fetch_memory(airbnb_url)$content
  read_airbnb_parquet(arrow::BufferReader$create(raw_parquet))
}

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

build_listing_text <- function(data) {
  available_text_cols <- intersect(text_cols, names(data))

  if (!length(available_text_cols)) {
    data$text <- ""
    return(data)
  }

  data |>
    mutate(across(all_of(available_text_cols), ~ replace_na(as.character(.x), ""))) |>
    unite("text", all_of(available_text_cols), sep = " ", remove = TRUE, na.rm = TRUE)
}

tokenize_words <- function(data) {
  data |>
    select(all_of(c("row_id", "text"))) |>
    unnest_tokens("word", "text")
}

tokenize_bigrams <- function(data, stop_words) {
  data |>
    select(all_of(c("row_id", "text"))) |>
    unnest_tokens("bigram", "text", token = "ngrams", n = 2) |>
    separate("bigram", c("word1", "word2"), sep = " ", fill = "right") |>
    filter(!.data$word1 %in% stop_words, !.data$word2 %in% stop_words) |>
    unite("bigram", c("word1", "word2"), sep = " ")
}

add_counts_to_env <- function(values, store) {
  if (!length(values)) return(invisible())

  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(invisible())

  counts <- table(values, useNA = "no")
  keys <- names(counts)
  for (i in seq_along(keys)) {
    key <- keys[[i]]
    current <- store[[key]]
    increment <- as.numeric(counts[[i]])
    store[[key]] <- if (is.null(current)) increment else current + increment
  }

  invisible()
}

env_counts_to_tibble <- function(store, key_name) {
  keys <- ls(store, all.names = TRUE)
  if (!length(keys)) {
    return(tibble::tibble())
  }

  counts <- vapply(keys, function(k) as.numeric(store[[k]]), numeric(1))
  out <- tibble::tibble(key = keys, n = counts)
  names(out)[1] <- key_name
  out
}

compute_text_metrics <- function(data, stop_words, afinn_lex, bing_lex, nrc_lex, top_n = 10, chunk_size = 250) {
  empty_result <- list(
    token_count = 0,
    word_top = tibble::tibble(word = character(), n = numeric()),
    bigram_top = tibble::tibble(bigram = character(), n = numeric()),
    sentiment_scores = tibble::tibble(method = character(), sentiment = numeric()),
    bing_top = tibble::tibble(sentiment = character(), word = character(), n = numeric())
  )

  if (!nrow(data)) return(empty_result)

  word_counts <- new.env(hash = TRUE, parent = emptyenv())
  bigram_counts <- new.env(hash = TRUE, parent = emptyenv())
  bing_word_counts <- new.env(hash = TRUE, parent = emptyenv())

  token_count <- 0
  afinn_score <- 0
  bing_counts <- c(positive = 0, negative = 0)
  nrc_counts <- c(positive = 0, negative = 0)

  split_index <- split(seq_len(nrow(data)), ceiling(seq_len(nrow(data)) / chunk_size))

  for (idx in split_index) {
    chunk <- data[idx, c("row_id", "text"), drop = FALSE]

    words_chunk <- tokenize_words(chunk)
    if (nrow(words_chunk)) {
      filtered_words <- words_chunk$word[!words_chunk$word %in% stop_words]
      token_count <- token_count + length(filtered_words)
      add_counts_to_env(filtered_words, word_counts)

      afinn_chunk <- inner_join(words_chunk, afinn_lex, by = "word")
      if (nrow(afinn_chunk)) {
        afinn_score <- afinn_score + sum(afinn_chunk$value)
      }

      bing_chunk <- inner_join(words_chunk, bing_lex, by = "word")
      if (nrow(bing_chunk)) {
        sentiment_totals <- table(bing_chunk$sentiment, useNA = "no")
        for (sent in names(sentiment_totals)) {
          if (sent %in% names(bing_counts)) {
            bing_counts[[sent]] <- bing_counts[[sent]] + as.numeric(sentiment_totals[[sent]])
          }
        }

        add_counts_to_env(paste(bing_chunk$sentiment, bing_chunk$word, sep = "\r"), bing_word_counts)
      }

      nrc_chunk <- inner_join(words_chunk, nrc_lex, by = "word", relationship = "many-to-many")
      if (nrow(nrc_chunk)) {
        sentiment_totals <- table(nrc_chunk$sentiment, useNA = "no")
        for (sent in names(sentiment_totals)) {
          if (sent %in% names(nrc_counts)) {
            nrc_counts[[sent]] <- nrc_counts[[sent]] + as.numeric(sentiment_totals[[sent]])
          }
        }
      }
    }

    bigrams_chunk <- tokenize_bigrams(chunk, stop_words)
    if (nrow(bigrams_chunk)) {
      add_counts_to_env(bigrams_chunk$bigram, bigram_counts)
    }

    rm(chunk, words_chunk, filtered_words, afinn_chunk, bing_chunk, nrc_chunk, bigrams_chunk)
    invisible(gc(FALSE))
  }

  word_top <- env_counts_to_tibble(word_counts, "word") |>
    arrange(desc(.data$n)) |>
    slice_head(n = top_n)

  bigram_top <- env_counts_to_tibble(bigram_counts, "bigram") |>
    arrange(desc(.data$n)) |>
    slice_head(n = top_n)

  bing_top <- env_counts_to_tibble(bing_word_counts, "key")
  if (nrow(bing_top)) {
    bing_top <- bing_top |>
      separate("key", c("sentiment", "word"), sep = "\r") |>
      group_by(.data$sentiment) |>
      arrange(desc(.data$n), .by_group = TRUE) |>
      slice_head(n = top_n) |>
      ungroup()
  } else {
    bing_top <- tibble::tibble(sentiment = character(), word = character(), n = numeric())
  }

  sentiment_scores <- tibble::tibble(
    method = c("AFINN", "Bing", "NRC"),
    sentiment = c(
      afinn_score,
      unname(bing_counts[["positive"]] - bing_counts[["negative"]]),
      unname(nrc_counts[["positive"]] - nrc_counts[["negative"]])
    )
  )

  list(
    token_count = token_count,
    word_top = word_top,
    bigram_top = bigram_top,
    sentiment_scores = sentiment_scores,
    bing_top = bing_top
  )
}

airbnb_data <- load_airbnb() |>
  mutate(row_id = row_number()) |>
  build_listing_text() |>
  mutate(
    text = trimws(text),
    longitude = parse_coordinates(.data[["address.location.coordinates"]], 1),
    latitude = parse_coordinates(.data[["address.location.coordinates"]], 2)
  ) |>
  select(any_of(c(
    "row_id",
    "text",
    "longitude",
    "latitude",
    "address.country",
    "address.market",
    "room_type",
    "price",
    "host.host_is_superhost",
    "review_scores.review_scores_rating",
    "listing_url",
    "images.picture_url",
    "name",
    "address.street",
    "property_type"
  )))

invisible(gc())

stop_words_vec <- tidytext::stop_words$word

ensure_textdata_cache <- function() {
  if (interactive()) return(invisible())

  cache_dir <- rappdirs::user_cache_dir("textdata")
  targets <- list(
    list(data_name = "afinn", file_name = "afinn_111.rds"),
    list(data_name = "bing", file_name = "bing.rds"),
    list(data_name = "nrc", file_name = "NRCWordEmotion.rds")
  )

  for (target in targets) {
    folder_path <- file.path(cache_dir, target$data_name)
    output_path <- file.path(folder_path, target$file_name)

    if (file.exists(output_path)) next

    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

    tryCatch(
      {
        textdata:::download_functions[[target$data_name]](folder_path)
        textdata:::process_functions[[target$data_name]](folder_path, output_path)
      },
      error = function(e) {
        stop(
          paste0(
            "Failed to prepare textdata cache for ", target$data_name,
            ". Underlying error: ", conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }

  invisible()
}

load_sentiment_lexicons <- function() {
  if (!requireNamespace("textdata", quietly = TRUE)) {
    stop(
      "Package 'textdata' is required to load sentiment lexicons on shinyapps.io.",
      call. = FALSE
    )
  }

  ensure_textdata_cache()

  list(
    afinn = textdata::lexicon_afinn(),
    bing = textdata::lexicon_bing(),
    nrc = textdata::lexicon_nrc() |>
      filter(.data$sentiment %in% c("positive", "negative"))
  )
}

lexicons <- load_sentiment_lexicons()
afinn_lex <- lexicons$afinn
bing_lex  <- lexicons$bing
nrc_lex   <- lexicons$nrc

countries <- sort(unique(stats::na.omit(airbnb_data[["address.country"]])))
cities <- sort(unique(stats::na.omit(airbnb_data[["address.market"]])) )
room_types <- sort(unique(stats::na.omit(airbnb_data$room_type)))
price_limits <- range(airbnb_data$price, na.rm = TRUE)
top_terms_n <- 10

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body { background-color: #e9ecef; }
            h2 { color: #2c3e50; }

            #app-header {
                display: flex;
                align-items: center;
                gap: 12px;
                margin: 6px 0 14px;
            }
            #app-header img {
                width: 44px;
                height: 44px;
                object-fit: contain;
            }
            #app-header h2 {
                margin: 0;
                font-size: 24px;
                line-height: 1.25;
                color: #2c3e50;
            }

            #app-layout { display: flex; gap: 18px; align-items: flex-start; }
            #sidebar-column { width: 26%; min-width: 260px; }
            #sidebar-column.sidebar-fixed {
                position: fixed;
                top: 18px;
                max-height: calc(100vh - 36px);
                overflow-y: auto;
                z-index: 1000;
            }
            #sidebar-placeholder { display: none; flex-shrink: 0; }
            #sidebar-column .well {
                background-color: #2c3e50;
                color: #ecf0f1;
                border: none;
                border-radius: 8px;
            }
            #sidebar-column .well label,
            #sidebar-column .well .control-label,
            #sidebar-column .well .help-block {
                color: #dce1e6;
            }

            #main-column { width: 74%; }
            #main-column .well {
                background-color: #ffffff;
                border: 1px solid #dee2e6;
                border-radius: 8px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.08);
            }
            #main-column .tab-content {
                background-color: #ffffff;
                border: 1px solid #dee2e6;
                border-top: none;
                border-radius: 0 0 8px 8px;
                padding: 12px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.08);
            }
            .nav-tabs > li.active > a,
            .nav-tabs > li.active > a:hover,
            .nav-tabs > li.active > a:focus {
                background-color: #ffffff;
                border-bottom-color: #ffffff;
            }
            .shiny-plot-output,
            .plotly.html-widget-output {
                background-color: #ffffff;
                border: 1px solid #dee2e6;
                border-radius: 8px;
                padding: 8px;
                margin-bottom: 12px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.08);
            }
            .tab-content .row .col-sm-6 {
                padding-left: 8px;
                padding-right: 8px;
            }

            #toggle_sidebar {
                margin-bottom: 12px;
                background-color: #2c3e50;
                color: #ffffff;
                border: none;
                border-radius: 6px;
            }
            #toggle_sidebar:hover { background-color: #34495e; }

            body.sidebar-collapsed #sidebar-column { display: none; }
            body.sidebar-collapsed #sidebar-placeholder { display: none !important; }
            body.sidebar-collapsed #main-column { width: 100%; }
            @media (max-width: 992px) {
                #app-layout { display: block; }
                #sidebar-column, #main-column { width: 100%; }
                body.sidebar-collapsed #sidebar-column { display: none; }
            }
        ")),
    tags$script(HTML("
            Shiny.addCustomMessageHandler('toggleSidebar', function(message) {
                document.body.classList.toggle('sidebar-collapsed');
            });

            $(document).ready(function() {
                var $sidebar = $('#sidebar-column');
                var $placeholder = $('<div id=\"sidebar-placeholder\"></div>').insertBefore($sidebar);
                var anchorTop = $sidebar.offset().top;

                function onScroll() {
                    if ($(window).scrollTop() + 18 > anchorTop) {
                        if (!$sidebar.hasClass('sidebar-fixed')) {
                            var w = $sidebar.outerWidth();
                            $placeholder.css({display:'block', width:w+'px', minWidth:w+'px'});
                            $sidebar.css('width', w+'px');
                            $sidebar.addClass('sidebar-fixed');
                        }
                    } else {
                        if ($sidebar.hasClass('sidebar-fixed')) {
                            $sidebar.removeClass('sidebar-fixed').css('width', '');
                            $placeholder.css('display', 'none');
                        }
                    }
                }

                $(window).on('scroll', onScroll);
                $(window).on('resize', function() {
                    $sidebar.removeClass('sidebar-fixed').css('width', '');
                    $placeholder.css('display', 'none');
                    anchorTop = $sidebar.offset().top;
                    onScroll();
                });
            });
        "))
  ),
  div(
    id = "app-header",
    tags$img(
      src = "https://tse4.mm.bing.net/th/id/OIP.fy97ig0fRI4BlT95a3nqqwHaHw?w=840&h=880&rs=1&pid=ImgDetMain&o=7&rm=3",
      alt = "Airbnb logo"
    ),
    tags$h2("Airbnb Text Mining and NLP | Business Analysis with Unstructured Data | Spring 2026")
  ),
  actionButton("toggle_sidebar", "Hide / Show Filters"),
  div(
    id = "app-layout",
    div(
      id = "sidebar-column",
      wellPanel(
        selectInput("country", "Country:", c("All", countries), selected = "All"),
        selectInput("city", "City:", c("All", cities), selected = "All"),
        selectInput("room_type", "Room type:", c("All", room_types), selected = "All"),
        sliderInput(
          "price_range",
          "Nightly price range:",
          min = floor(price_limits[1]),
          max = ceiling(price_limits[2]),
          value = c(floor(price_limits[1]), 1000),
          pre = "$"
        ),
        checkboxInput("superhost_only", "Only show superhosts", FALSE),
        helpText("")
      )
    ),
    div(
      id = "main-column",
      fluidRow(
        column(3, wellPanel(h4("Listings"), textOutput("listing_count"))),
        column(3, wellPanel(h4("Average price"), textOutput("avg_price"))),
        column(3, wellPanel(h4("Average rating"), textOutput("avg_rating"))),
        column(3, wellPanel(h4("Tokens"), textOutput("token_count")))
      ),
      fluidRow(
        column(
          12,
          tabsetPanel(
            tabPanel("Map", leaflet::leafletOutput("map", height = 520)),
            tabPanel("Table", DTOutput("listing_table"))
          )
        )
      ),
      fluidRow(
        column(
          12,
          tabsetPanel(
            tabPanel(
              "Frequency Distributions",
              plotlyOutput("price_plot"),
              fluidRow(
                column(6, plotlyOutput("word_plot")),
                column(6, plotlyOutput("bigram_plot"))
              )
            ),
            tabPanel("Sentiment", plotlyOutput("sentiment_plot"), plotlyOutput("bing_plot"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_sidebar, {
    session$sendCustomMessage("toggleSidebar", list())
  })
  
  # --- Debounced filtered data (avoids rapid recalc on slider drag) ---
  filtered_data_raw <- reactive({
    data <- airbnb_data |>
      filter(
        !is.na(.data$price),
        .data$price >= input$price_range[1],
        .data$price <= input$price_range[2],
        .data$text != ""
      )
    
    if (input$country != "All") data <- filter(data, .data[["address.country"]] == input$country)
    if (input$city != "All") data <- filter(data, .data[["address.market"]] == input$city)
    if (input$room_type != "All") data <- filter(data, .data$room_type == input$room_type)
    
    if (isTRUE(input$superhost_only) && "host.host_is_superhost" %in% names(data)) {
      data <- filter(data, .data[["host.host_is_superhost"]] %in% c(TRUE, "TRUE", "True", "t"))
    }
    
    data
  })
  
  filtered_data <- filtered_data_raw |> debounce(300)

  text_metrics <- reactive({
    compute_text_metrics(
      data = filtered_data(),
      stop_words = stop_words_vec,
      afinn_lex = afinn_lex,
      bing_lex = bing_lex,
      nrc_lex = nrc_lex,
      top_n = top_terms_n,
      chunk_size = 250
    )
  })
  
  mapped_data <- reactive({
    filtered_data() |>
      filter(!is.na(.data$longitude), !is.na(.data$latitude))
  })
  
  output$listing_count <- renderText(scales::comma(nrow(filtered_data())))
  
  output$avg_price <- renderText({
    data <- filtered_data()
    if (!nrow(data)) return("No data")
    scales::dollar(mean(data$price, na.rm = TRUE))
  })
  
  output$avg_rating <- renderText({
    data <- filtered_data()
    rating <- mean(data[["review_scores.review_scores_rating"]], na.rm = TRUE)
    if (!nrow(data) || is.nan(rating)) return("No ratings")
    round(rating, 1)
  })
  
  output$token_count <- renderText(scales::comma(text_metrics()$token_count))
  
  # --- Map: render base tiles once, update markers via proxy ---
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles()
  })
  
  observe({
    data <- mapped_data()
    
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearGroup("listings")
    
    if (nrow(data) == 0) return()
    
    rating_label <- ifelse(
      is.na(data[["review_scores.review_scores_rating"]]),
      "N/A",
      round(data[["review_scores.review_scores_rating"]], 1)
    )

    url_html <- ifelse(
      is.na(data$listing_url) | data$listing_url == "",
      "",
      paste0("<br><a href=\"", data$listing_url, "\" target=\"_blank\" style=\"color:#2C7FB8;\">View on Airbnb</a>")
    )

    popup_html <- paste0(
      "<strong>", htmltools::htmlEscape(data$name), "</strong><br>",
      "Price: ", scales::dollar(data$price), "<br>",
      "Rating: ", rating_label,
      url_html
    ) |> lapply(htmltools::HTML)
    
    proxy |>
      leaflet::addCircleMarkers(
        data = data,
        group = "listings",
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(5, pmin(14, price / 100)),
        stroke = FALSE,
        fillOpacity = 0.7,
        color = "#2C7FB8",
        popup = popup_html,
        clusterOptions = leaflet::markerClusterOptions()
      ) |>
      leaflet::fitBounds(
        lng1 = min(data$longitude, na.rm = TRUE),
        lat1 = min(data$latitude, na.rm = TRUE),
        lng2 = max(data$longitude, na.rm = TRUE),
        lat2 = max(data$latitude, na.rm = TRUE)
      )
  })
  
  output$listing_table <- renderDT({
    filtered_data() |>
      transmute(
        name = .data$name,
        country = .data[["address.country"]],
        market = .data[["address.market"]],
        property_type = .data$property_type,
        room_type = .data$room_type,
        price = scales::dollar(.data$price),
        rating = round(.data[["review_scores.review_scores_rating"]], 1),
        image_url = .data[["images.picture_url"]],
        url = ifelse(
          is.na(.data$listing_url) | .data$listing_url == "",
          "",
          paste0("<a href='", .data$listing_url, "' target='_blank'>View</a>")
        )
      ) |>
      datatable(
        rownames = FALSE,
        escape = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          columnDefs = list(list(targets = 7, visible = FALSE)),
          drawCallback = JS(
            "function(settings) {",
            "  var api = this.api();",
            "  var $rows = $('[data-toggle=\"tooltip\"]', api.table().body());",
            "  $rows.tooltip('destroy');",
            "  $rows.tooltip({container: 'body', html: true, trigger: 'hover', placement: 'auto'});",
            "}"
          ),
          createdRow = JS(
            "function(row, data, dataIndex) {",
            "  var rating = (data[6] === null || data[6] === '' || typeof data[6] === 'undefined') ? 'N/A' : data[6];",
            "  var image = data[7] || '';",
            "  var tip = '<div style=\"max-width:100px;\">'",
            "    + '<strong>' + data[0] + '</strong><br>'",
            "    + data[4] + ' | ' + data[3] + '<br>'",
            "    + 'Price: ' + data[5] + ' | Rating: ' + rating",
            "    + (image ? '<br><img src=\"' + image + '\" style=\"width:80px;max-width:100%;height:auto;margin-top:6px;border-radius:6px;\">' : '')",
            "    + '</div>';",
            "  $(row)",
            "    .attr('data-toggle', 'tooltip')",
            "    .attr('data-html', 'true')",
            "    .attr('data-placement', 'auto')",
            "    .attr('title', tip);",
            "}"
          )
        )
      )
  })
  
  output$price_plot <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No listings match the selected filters."))

    x <- NULL
    
    p <- ggplot(data, aes(.data$price)) +
      geom_histogram(bins = 30, fill = "#2C7FB8", color = "white",
                     aes(text = paste0("Price: ", scales::dollar(after_stat(x)),
                                       "<br>Count: ", scales::comma(after_stat(count))))) +
      scale_x_continuous(labels = scales::label_dollar()) +
      labs(title = "Nightly Price Distribution", x = "Nightly price", y = "Listings") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })
  
  output$word_plot <- renderPlotly({
    data <- text_metrics()$word_top |>
      mutate(word = reorder(.data$word, .data$n))
    
    validate(need(nrow(data) > 0, "No tokens available for the selected filters."))
    
    p <- ggplot(data, aes(.data$word, .data$n)) +
      geom_col(fill = "#41AE76") +
      coord_flip() +
      labs(title = "Most Frequent Words", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })
  
  output$bigram_plot <- renderPlotly({
    data <- text_metrics()$bigram_top |>
      mutate(bigram = reorder(.data$bigram, .data$n))
    
    validate(need(nrow(data) > 0, "No bigrams available for the selected filters."))
    
    p <- ggplot(data, aes(.data$bigram, .data$n)) +
      geom_col(fill = "#DD6E42") +
      coord_flip() +
      labs(title = "Most Frequent Bigrams", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })
  
  output$sentiment_plot <- renderPlotly({
    data <- text_metrics()$sentiment_scores
    
    validate(need(any(is.finite(data$sentiment)), "No sentiment words were found for the selected filters."))
    
    p <- ggplot(data, aes(.data$method, .data$sentiment, fill = .data$method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y") +
      labs(title = "Sentiment by Lexicon", x = NULL, y = "Sentiment score") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })
  
  output$bing_plot <- renderPlotly({
    data <- text_metrics()$bing_top |>
      mutate(word = reorder(.data$word, .data$n))
    
    validate(need(nrow(data) > 0, "No positive or negative words were found for the selected filters."))
    
    p <- ggplot(data, aes(.data$word, .data$n, fill = .data$sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() +
      labs(title = "Most Common Positive and Negative Words", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)
