library(shiny)
library(arrow)
library(curl)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

airbnb_url <- paste0(
    "https://github.com/renan-peres/mban-unstructured-data/raw/",
    "refs/heads/main/%23A1%20-%20AirBnb%20Text%20Mining%20%26%20NLP/",
    "data/airbnb_all.parquet"
)

airbnb_data <- local({
    # Parquet needs random access, so we read the remote file into memory
    # instead of writing it to disk before passing it to Arrow.
    raw_parquet <- curl::curl_fetch_memory(airbnb_url)$content
    arrow::read_parquet(
        arrow::BufferReader$create(raw_parquet),
        as_data_frame = TRUE
    )
})

countries <- sort(unique(stats::na.omit(airbnb_data[["address.country"]])))
room_types <- sort(unique(stats::na.omit(airbnb_data$room_type)))
price_limits <- range(airbnb_data$price, na.rm = TRUE)
default_max_price <- min(1000, ceiling(price_limits[2]))

ui <- fluidPage(
    titlePanel("Airbnb Listings Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "country",
                "Country:",
                choices = c("All", countries),
                selected = "All"
            ),
            selectInput(
                "room_type",
                "Room type:",
                choices = c("All", room_types),
                selected = "All"
            ),
            sliderInput(
                "price_range",
                "Nightly price range:",
                min = floor(price_limits[1]),
                max = ceiling(price_limits[2]),
                value = c(floor(price_limits[1]), default_max_price),
                pre = "$"
            ),
            checkboxInput("superhost_only", "Only show superhosts", value = FALSE),
            helpText(
                "The parquet file is read directly from GitHub into memory.",
                "No local parquet file is created."
            )
        ),
        mainPanel(
            fluidRow(
                column(4, wellPanel(h4("Listings"), textOutput("listing_count"))),
                column(4, wellPanel(h4("Average price"), textOutput("avg_price"))),
                column(4, wellPanel(h4("Average rating"), textOutput("avg_rating")))
            ),
            plotOutput("price_plot"),
            plotOutput("room_type_plot"),
            DTOutput("listing_table")
        )
    )
)

server <- function(input, output) {

    filtered_data <- reactive({
        data <- airbnb_data |>
            filter(
                !is.na(price),
                price >= input$price_range[1],
                price <= input$price_range[2]
            )

        if (input$country != "All") {
            data <- data |>
                filter(.data[["address.country"]] == input$country)
        }

        if (input$room_type != "All") {
            data <- data |>
                filter(room_type == input$room_type)
        }

        if (isTRUE(input$superhost_only) && "host.host_is_superhost" %in% names(data)) {
            data <- data |>
                filter(.data[["host.host_is_superhost"]] %in% c(TRUE, "TRUE", "True", "t"))
        }

        data
    })

    output$listing_count <- renderText({
        scales::comma(nrow(filtered_data()))
    })

    output$avg_price <- renderText({
        data <- filtered_data()

        if (!nrow(data)) {
            return("No data")
        }

        paste0("$", scales::comma(round(mean(data$price, na.rm = TRUE), 2)))
    })

    output$avg_rating <- renderText({
        data <- filtered_data()
        rating <- mean(data[["review_scores.review_scores_rating"]], na.rm = TRUE)

        if (!nrow(data) || is.nan(rating)) {
            return("No ratings")
        }

        round(rating, 1)
    })

    output$price_plot <- renderPlot({
        data <- filtered_data()

        validate(need(nrow(data) > 0, "No listings match the selected filters."))

        ggplot(data, aes(x = price)) +
            geom_histogram(bins = 30, fill = "#2C7FB8", color = "white") +
            scale_x_continuous(labels = scales::label_dollar()) +
            labs(
                title = "Distribution of Nightly Prices",
                x = "Nightly price",
                y = "Listings"
            ) +
            theme_minimal(base_size = 13)
    })

    output$room_type_plot <- renderPlot({
        data <- filtered_data() |>
            count(room_type, sort = TRUE)

        validate(need(nrow(data) > 0, "No room types available for the selected filters."))

        ggplot(data, aes(x = reorder(room_type, n), y = n)) +
            geom_col(fill = "#41AE76") +
            coord_flip() +
            labs(
                title = "Listings by Room Type",
                x = "Room type",
                y = "Listings"
            ) +
            theme_minimal(base_size = 13)
    })

    output$listing_table <- renderDT({
        data <- filtered_data() |>
            transmute(
                name,
                country = .data[["address.country"]],
                market = .data[["address.market"]],
                property_type,
                room_type,
                price,
                rating = .data[["review_scores.review_scores_rating"]],
                listing_url
            )

        datatable(
            data,
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
}

shinyApp(ui = ui, server = server)
