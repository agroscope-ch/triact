

return_data <- function(value) {
  triact_class <- getOption("triact_class", default = "data.frame")
  checkmate::checkChoice(triact_class, choices = c("data.frame", "data.table", "tibble"))
  if (missing(value)) {
    if (triact_class == "data.frame") {
      return(as.data.frame(private$dataDT))
    } else if (triact_class == "data.table") {
      return(private$dataDT)
    } else if (triact_class == "tibble") {
      return(tibble::as_tibble(private$dataDT))
    }
  } else private$dataDT <- data.table::as.data.table(value)
}


