# Install Packages
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

countries <- sort(unique(stats::na.omit(airbnb_data[["address.country"]])))
cities <- sort(unique(stats::na.omit(airbnb_data[["address.market"]])) )
room_types <- sort(unique(stats::na.omit(airbnb_data$room_type)))
price_limits <- range(airbnb_data$price, na.rm = TRUE)
top_terms_n <- 10

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n            #app-layout { display: flex; gap: 18px; align-items: flex-start; }\n            #sidebar-column { width: 26%; min-width: 260px; }\n            #main-column { width: 74%; }\n            #toggle_sidebar { margin-bottom: 12px; }\n            body.sidebar-collapsed #sidebar-column { display: none; }\n            body.sidebar-collapsed #main-column { width: 100%; }\n            @media (max-width: 992px) {\n                #app-layout { display: block; }\n                #sidebar-column, #main-column { width: 100%; }\n                body.sidebar-collapsed #sidebar-column { display: none; }\n            }\n        ")),
    tags$script(HTML("\n            Shiny.addCustomMessageHandler('toggleSidebar', function(message) {\n                document.body.classList.toggle('sidebar-collapsed');\n            });\n        "))
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
          value = c(floor(price_limits[1]), min(1000, ceiling(price_limits[2]))),
          pre = "$"
        ),
        checkboxInput("superhost_only", "Only show superhosts", FALSE),
        helpText("NLP uses listing-description fields covered in class tokenization, n-grams and sentiment exercises.")
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
              "Classes Frequency",
              plotOutput("price_plot"),
              fluidRow(
                column(6, plotOutput("word_plot")),
                column(6, plotOutput("bigram_plot"))
              )
            ),
            tabPanel("Sentiment", plotOutput("sentiment_plot"), plotOutput("bing_plot"))
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
  
  filtered_data <- reactive({
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
  
  words <- reactive({
    filtered_data() |>
      select(all_of(c("row_id", "text"))) |>
      unnest_tokens("word", "text")
  })
  
  tokens <- reactive({
    words() |>
      anti_join(tidytext::stop_words, by = "word")
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
  
  output$map <- leaflet::renderLeaflet({
    data <- mapped_data()
    validate(need(nrow(data) > 0, "No listings with coordinates match the selected filters."))
    
    popup_html <- ifelse(
      is.na(data[["images.picture_url"]]) | data[["images.picture_url"]] == "",
      paste0(
        "<strong>", data$name, "</strong><br>",
        data[["address.street"]], "<br>",
        data[["address.country"]], "<br>",
        data$room_type, " | ", data$property_type, "<br>",
        "Price: ", scales::dollar(data$price)
      ),
      paste0(
        "<strong>", data$name, "</strong><br>",
        "<img src=\"", data[["images.picture_url"]],
        "\" style=\"width:220px;max-width:100%;height:auto;margin:8px 0;border-radius:6px;\"><br>",
        data[["address.street"]], "<br>",
        data[["address.country"]], "<br>",
        data$room_type, " | ", data$property_type, "<br>",
        "Price: ", scales::dollar(data$price)
      )
    )
    
    leaflet::leaflet(data) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(5, pmin(14, price / 100)),
        stroke = FALSE,
        fillOpacity = 0.7,
        color = "#2C7FB8",
        popup = popup_html,
        popupOptions = leaflet::popupOptions(
          offset = c(0, 18),
          autoPan = TRUE,
          closeButton = TRUE
        )
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
        rating = round(.data[["review_scores.review_scores_rating"]], 1)
      ) |>
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$price_plot <- renderPlot({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No listings match the selected filters."))
    
    ggplot(data, aes(.data$price)) +
      geom_histogram(bins = 30, fill = "#2C7FB8", color = "white") +
      scale_x_continuous(labels = scales::label_dollar()) +
      labs(title = "Nightly Price Distribution", x = "Nightly price", y = "Listings") +
      theme_minimal(base_size = 12)
  })
  
  output$word_plot <- renderPlot({
    data <- tokens() |>
      count(.data$word, sort = TRUE) |>
      slice_head(n = top_terms_n) |>
      mutate(word = reorder(.data$word, .data$n))
    
    validate(need(nrow(data) > 0, "No tokens available for the selected filters."))
    
    ggplot(data, aes(.data$word, .data$n)) +
      geom_col(fill = "#41AE76") +
      coord_flip() +
      labs(title = "Most Frequent Words", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
  })
  
  output$bigram_plot <- renderPlot({
    data <- filtered_data() |>
      select(all_of(c("row_id", "text"))) |>
      unnest_tokens("bigram", "text", token = "ngrams", n = 2) |>
      separate("bigram", c("word1", "word2"), sep = " ") |>
      filter(
        !.data$word1 %in% tidytext::stop_words$word,
        !.data$word2 %in% tidytext::stop_words$word
      ) |>
      count(.data$word1, .data$word2, sort = TRUE) |>
      unite("bigram", c("word1", "word2"), sep = " ") |>
      slice_head(n = top_terms_n) |>
      mutate(bigram = reorder(.data$bigram, .data$n))
    
    validate(need(nrow(data) > 0, "No bigrams available for the selected filters."))
    
    ggplot(data, aes(.data$bigram, .data$n)) +
      geom_col(fill = "#DD6E42") +
      coord_flip() +
      labs(title = "Most Frequent Bigrams", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
  })
  
  output$sentiment_plot <- renderPlot({
    data <- bind_rows(
      words() |>
        inner_join(get_sentiments("afinn"), by = "word") |>
        summarise(sentiment = sum(.data$value)) |>
        mutate(method = "AFINN"),
      bind_rows(
        words() |>
          inner_join(get_sentiments("bing"), by = "word") |>
          mutate(method = "Bing"),
        words() |>
          inner_join(
            get_sentiments("nrc") |> filter(.data$sentiment %in% c("positive", "negative")),
            by = "word",
            relationship = "many-to-many"
          ) |>
          mutate(method = "NRC")
      ) |>
        count(.data$method, .data$sentiment) |>
        spread("sentiment", "n", fill = 0) |>
        mutate(sentiment = .data$positive - .data$negative) |>
        select(all_of(c("method", "sentiment")))
    )
    
    validate(need(any(is.finite(data$sentiment)), "No sentiment words were found for the selected filters."))
    
    ggplot(data, aes(.data$method, .data$sentiment, fill = .data$method)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~method, ncol = 1, scales = "free_y") +
      labs(title = "Sentiment by Lexicon", x = NULL, y = "Sentiment score") +
      theme_minimal(base_size = 12)
  })
  
  output$bing_plot <- renderPlot({
    data <- words() |>
      inner_join(get_sentiments("bing"), by = "word") |>
      count(.data$sentiment, .data$word, sort = TRUE) |>
      group_by(.data$sentiment) |>
      slice_head(n = top_terms_n) |>
      ungroup() |>
      mutate(word = reorder(.data$word, .data$n))
    
    validate(need(nrow(data) > 0, "No positive or negative words were found for the selected filters."))
    
    ggplot(data, aes(.data$word, .data$n, fill = .data$sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() +
      labs(title = "Most Common Positive and Negative Words", x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui = ui, server = server)
