# ----------------------------------------------------------------
# active field 'data' of Triact class
# ----------------------------------------------------------------

return_data <- function(value) {
  if (missing(value)) {
    transform_table(private$dataDT)
  } else self$load_table(value)
}
