# ----------------------------------------------------------------
# active field 'data' of Triact class
# ----------------------------------------------------------------

return_data <- function(value) {
  if (missing(value)) {
    transform_table(private$dataDT)
  } else private$dataDT <- data.table::as.data.table(value)
}
