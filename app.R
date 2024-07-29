library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(DT)
library(plotly)

ui <- page_fillable(
  layout_columns(
    card(
      card_header("Outliers"),
      markdown("Change `Flag` to `1` to flag a value as an error."),
      DTOutput("table"),
      actionButton("write_data", "Write to database", width = "50%"),
    ),
    card(plotlyOutput("plot"))
  )
)

server <- function(input, output, session) {
  # Connect to DuckDB
  con <- dbConnect(duckdb::duckdb(), dbdir = "data/ozone.duckdb")

  # Query data from DuckDB
  #data <- reactive({
    ozone <- dplyr::tbl(con, "ozone") |> collect()
  #})


  #outliers <- reactive({
    # data_df <- data()
    # iqr_bound <- 1.5 * IQR(data_df$arithmetic_mean)
    # q1 <- quantile(data_df$arithmetic_mean, 0.25)
    # q3 <- quantile(data_df$arithmetic_mean, 0.75)
    iqr_bound <- 1.5 * IQR(ozone$arithmetic_mean)
    q1 <- quantile(ozone$arithmetic_mean, 0.25)
    q3 <- quantile(ozone$arithmetic_mean, 0.75)
    outliers <-
      #data_df |>
      ozone |>
      filter(
        arithmetic_mean > (q3 + iqr_bound) |
          arithmetic_mean < (q1 - iqr_bound)
      )
 # })

  selectedRow <- reactiveVal(NULL)

  # Plot data
  output$plot <- renderPlotly({
    p <-
      ggplot(mapping = aes(date_local, arithmetic_mean)) +
      geom_point(
        # data = data() |> anti_join(outliers(), by = join_by("id")),
        data = ozone |> anti_join(outliers, by = join_by("id")),
        alpha = 0.4,
        size = 2
      ) +
      geom_point(
        # data = outliers(),
        data = outliers,
        alpha = 0.4,
        size = 2,
        color = "red"
      ) +
      theme_minimal() +
      labs(
        x = "Date",
        y = "Mean ppm"
      )

    ggplotly(p) |>
      event_register("plotly_click")
  })

  # Editable datatable
  output$table <- renderDT({
    datatable(
      #outliers() |>
      outliers |>
        select(
          State = state_name,
          Date = date_local,
          PPM = arithmetic_mean
        ) |>
        mutate(Flag = 0L, PPM = round(PPM, 2)),
      options =
        list(
          searching = FALSE,
          lengthChange = FALSE,
          info = FALSE,
          paging = FALSE,
          scrolly = "400px"
        ),
      editable =
        list(target = "cell", disable = list(columns = 1:4)),
      rownames = FALSE
    )
  })


  observeEvent(event_data("plotly_click"), {
    data_click <- event_data("plotly_click")
    if (is.null(data_click)) return(NULL)

    point_clicked <- data_click$pointNumber + 1
    selectedRow(point_clicked)
  })

  observe({
    selected <- selectedRow()
    if (is.null(selected)) return(NULL)
    proxy <- dataTableProxy("table")
    selectRows(proxy, selected)
  })


  # Disconnect from DuckDB when the app stops
  onSessionEnded(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

shinyApp(ui, server)