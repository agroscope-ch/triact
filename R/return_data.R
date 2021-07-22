

return_data <- function(value) {
  if (missing(value)) {
    transform_table(private$dataDT)
  } else private$dataDT <- data.table::as.data.table(value)
}

transform_table <- function(x, table_class = getOption("triact_table", default = "data.frame")) {
  checkmate::checkChoice(table_class, choices = c("data.frame", "data.table", "tibble"))
  if (table_class == "data.frame") {
    return(as.data.frame(x))
  } else if (table_class == "data.table") {
    return(data.table::as.data.table(x))
  } else if (table_class == "tibble") {
    return(tibble::as_tibble(x))
  }
}
