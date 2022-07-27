# ----------------------------------------------------------------
# internal function used in all methods of the Triact class which return a table
# transforms a table to data.frame, tibble, or data.table depending on global var 'triact_table'
# ----------------------------------------------------------------

transform_table <- function(x, table_class = getOption("triact_table", default = "data.frame")) {
  checkmate::assertChoice(table_class, choices = c("data.frame", "data.table", "tibble"), .var.name = "Global option triact_table")
  if (table_class == "data.frame") {
    return(as.data.frame(x))
  } else if (table_class == "data.table") {
    return(data.table::as.data.table(x))
  } else if (table_class == "tibble") {
    return(tibble::as_tibble(x))
  }
}
