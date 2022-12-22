################################################################################
# Triact class
################################################################################

Triact <- R6::R6Class("Triact",
  active = list(
    data = function(value) {
      if (missing(value)) {
        return(transform_table(private$dataDT))
      } else return(self$load_table(value))
    }
  ),
  public = list(
    load_files = load_files,
    load_table = load_table,
    check_orientation = check_orientation,
    add_lying = add_lying,
    add_side = add_side,
    add_activity = add_activity,
    extract_liedown = extract_liedown,
    extract_standup = extract_standup,
    summarize_intervals = summarize_intervals,
    summarize_bouts = summarize_bouts
  ),
  private = list(dataDT = NULL,
                 sampInt = NA,
                 filter_acc = filter_acc,
                 has = function(to_check) {
                   if (to_check[1] == "data") {
                     return(checkmate::testDataTable(private$dataDT))
                   } else {
                     return(to_check %in% colnames(private$dataDT))
                   }
                 },
                 # fix deep cloning R6 with reference type objects (here dataDT)
                 # gets invoked when x$clone(deep=TRUE) is called
                 # see https://r6.r-lib.org/articles/Introduction.html
                 deep_clone = function(name, value) {
                   if (name == "dataDT") {
                     return(data.table::copy(value))
                   } else {
                     return(value)
                   }
                 }
  )
)
