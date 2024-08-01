library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(DT)
library(plotly)
library(plu)
library(dplyr)

source("R/plot.R")
source("R/table.R")
source("R/data.R")

choices <-
  c(
    "Date" = "date_local",
    "PPM" = "ppm",
    "AQI" = "aqi"
  )

ui <- page_fluid(
  markdown("## Identify outliers in air quality data"),
  layout_column_wrap(
    card(
      card_header("Click on an outlier to highlight the point in the table."),
      layout_column_wrap(
        selectInput(
          "plot_x",
          "X-axis variable:",
          choices = choices
        ),
        selectInput(
          "plot_y",
          "Y-axis variable:",
          choices = choices[choices != "date_local"]
        )
      ),
      plotlyOutput("plot"),
      full_screen = TRUE
    ),
    card(
      card_header(markdown("Change `Flag` to `1` to flag a value as an error.")),
      DTOutput("table"),
      actionButton("write_data", "Write to database", width = "50%")
    )
  )
)

server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "data/ozone.duckdb")
  ozone <-
    dplyr::tbl(con, "ozone") |>
    collect() |>
    rename(ppm = arithmetic_mean)

  outliers <- create_outliers(ozone)

  selectedRow <- reactiveVal(NULL)

  # Reactive value to store the edited data
  edited_data <- reactiveVal(outliers)

  # Capture edits made in the DataTable
  observeEvent(
    input$table_cell_edit,
    {
      edited_cell <- input$table_cell_edit
      edited <- edited_data()

      new_flag_value <- as.integer(edited_cell$value)
      if (new_flag_value > 1 || new_flag_value < 0) {
        showNotification(
          markdown("Error: `flag` must be either `0` or `1`."),
          type = "error"
        )
      }

      else {
        edited[edited_cell$row, "flag"] <- new_flag_value
        edited_data(edited) # Update with new data
      }
    }
  )

  # Plot data
  output$plot <-
    renderPlotly({
      plot_ozone(input, ozone, edited_data(), plotly_event = "plotly_click")
    })

  # Editable datatable
  output$table <- renderDT({
    create_editable_table(edited_data())
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

  # Write data to database on button click
  observeEvent(
    input$write_data, {
      tryCatch(
        {
          dbBegin(con)

          rows_changed <-
            outliers |>
            left_join(
              edited_data(),
              by = join_by("id"),
              suffix = c("_old", "_new")
            ) |>
            filter(flag_old != flag_new) |>
            select(id, flag_new)


          for (id in rows_changed$id) {
            dbExecute(
              con,
              "UPDATE ozone SET flag = ? WHERE id = ?",
              params = list(rows_changed$flag_new[rows_changed$id == id], id)
            )
          }

          n_changes <- nrow(rows_changed)

          showNotification(
            markdown(
              glue::glue(
                "{n_changes} `Flag` {plu::ral('value', n_changes[n_changes == 1])} successfully updated in database."
              )
            ),
            type = "message"
          )

          dbCommit(con)
        },
        error = function(e) {
          dbRollback(con)
          showNotification("Error: Failed to update database.", type = "error")
        }
      )
    }
  )

  # Disconnect from DuckDB when the app stops
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
