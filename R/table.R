create_editable_table <- function(data) {
  datatable(
    data |>
      select(
        state_name,
        date_local,
        ppm,
        aqi,
        flag
      ) |>
      mutate(ppm = round(ppm, 2)),
    colnames = c("State", "Date", "PPM", "AQI", "Flag"),
    options =
      list(
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        paging = FALSE,
        scrollY = "400px"  # Fixed scroll area height
      ),
    editable = list(target = "cell", columns = 4),  # Make only flag column editable
    rownames = FALSE
  )
}