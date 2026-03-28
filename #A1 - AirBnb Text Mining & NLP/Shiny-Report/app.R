## Install Packages
# options(download.file.method = "libcurl")
# install.packages("leaflet", repos = "https://cloud.r-project.org")

library(shiny)
library(arrow)
library(curl)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(DT)
library(plotly)
library(scales)

airbnb_url <- paste0(
  "https://github.com/renan-peres/mban-unstructured-data/raw/",
  "refs/heads/main/%23A1%20-%20AirBnb%20Text%20Mining%20%26%20NLP/",
  "data/airbnb_all.parquet"
)

text_cols <- c(
  "summary", "space", "description", "neighborhood_overview",
  "notes", "transit", "access", "interaction", "house_rules"
)

load_airbnb <- function() {
  raw_parquet <- curl::curl_fetch_memory(airbnb_url)$content
  arrow::read_parquet(arrow::BufferReader$create(raw_parquet), as_data_frame = TRUE)
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

airbnb_data <- load_airbnb() |>
  mutate(row_id = row_number()) |>
  mutate(across(any_of(text_cols), ~ replace_na(as.character(.x), ""))) |>
  unite("text", any_of(text_cols), sep = " ", remove = FALSE, na.rm = TRUE) |>
  mutate(
    text = trimws(text),
    longitude = parse_coordinates(.data[["address.location.coordinates"]], 1),
    latitude = parse_coordinates(.data[["address.location.coordinates"]], 2)
  )

# --- Pre-tokenize and pre-join at startup for fast filtering ---
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

afinn_lex <- get_sentiments("afinn")
bing_lex  <- get_sentiments("bing")
nrc_lex   <- get_sentiments("nrc") |>
  filter(.data$sentiment %in% c("positive", "negative"))

all_afinn <- inner_join(all_words, afinn_lex, by = "word")
all_bing  <- inner_join(all_words, bing_lex, by = "word")
all_nrc   <- inner_join(all_words, nrc_lex, by = "word", relationship = "many-to-many")

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
  titlePanel("Airbnb Text Mining and NLP"),
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
  
  # --- Filter pre-computed tokens/sentiments by matching row_id ---
  filtered_ids <- reactive({
    filtered_data()$row_id
  })
  
  words <- reactive({
    ids <- filtered_ids()
    all_words[all_words$row_id %in% ids, ]
  })
  
  tokens <- reactive({
    ids <- filtered_ids()
    all_tokens[all_tokens$row_id %in% ids, ]
  })
  
  bigrams <- reactive({
    ids <- filtered_ids()
    all_bigrams[all_bigrams$row_id %in% ids, ]
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
  
  output$token_count <- renderText(scales::comma(nrow(tokens())))
  
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
      paste0("<a href=\"", data$listing_url, "\" target=\"_blank\" style=\"color:#2C7FB8;\">View on Airbnb</a><br>")
    )

    label_html <- ifelse(
      is.na(data[["images.picture_url"]]) | data[["images.picture_url"]] == "",
      paste0(
        "<strong>", data$name, "</strong><br>",
        data[["address.street"]], "<br>",
        data[["address.country"]], "<br>",
        data$room_type, " | ", data$property_type, "<br>",
        "Price: ", scales::dollar(data$price), "<br>",
        "Rating: ", rating_label, "<br>",
        url_html
      ),
      paste0(
        "<strong>", data$name, "</strong><br>",
        "<img src=\"", data[["images.picture_url"]],
        "\" style=\"width:220px;max-width:100%;height:auto;margin:8px 0;border-radius:6px;\"><br>",
        data[["address.street"]], "<br>",
        data[["address.country"]], "<br>",
        data$room_type, " | ", data$property_type, "<br>",
        "Price: ", scales::dollar(data$price), "<br>",
        "Rating: ", rating_label, "<br>",
        url_html
      )
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
        popup = label_html,
        label = label_html,
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-size" = "13px",
            "padding" = "8px 12px",
            "max-width" = "300px",
            "background-color" = "white",
            "border-radius" = "8px",
            "box-shadow" = "0 2px 6px rgba(0,0,0,0.15)"
          ),
          direction = "auto"
        )
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
    data <- tokens() |>
      count(.data$word, sort = TRUE) |>
      slice_head(n = top_terms_n) |>
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
    data <- bigrams() |>
      count(.data$bigram, sort = TRUE) |>
      slice_head(n = top_terms_n) |>
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
    ids <- filtered_ids()
    
    afinn_data <- all_afinn[all_afinn$row_id %in% ids, ]
    bing_data  <- all_bing[all_bing$row_id %in% ids, ]
    nrc_data   <- all_nrc[all_nrc$row_id %in% ids, ]
    
    data <- bind_rows(
      afinn_data |>
        summarise(sentiment = sum(.data$value)) |>
        mutate(method = "AFINN"),
      bind_rows(
        bing_data |> mutate(method = "Bing"),
        nrc_data |> mutate(method = "NRC")
      ) |>
        count(.data$method, .data$sentiment) |>
        spread("sentiment", "n", fill = 0) |>
        mutate(sentiment = .data$positive - .data$negative) |>
        select(all_of(c("method", "sentiment")))
    )
    
    validate(need(any(is.finite(data$sentiment)), "No sentiment words were found for the selected filters."))
    
    p <- ggplot(data, aes(.data$method, .data$sentiment, fill = .data$method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y") +
      labs(title = "Sentiment by Lexicon", x = NULL, y = "Sentiment score") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
  })
  
  output$bing_plot <- renderPlotly({
    bing_data <- all_bing[all_bing$row_id %in% filtered_ids(), ]
    
    data <- bing_data |>
      count(.data$sentiment, .data$word, sort = TRUE) |>
      group_by(.data$sentiment) |>
      slice_head(n = top_terms_n) |>
      ungroup() |>
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
