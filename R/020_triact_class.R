# ----------------------------------------------------------------
# Triact class
# ----------------------------------------------------------------

Triact <- R6::R6Class("Triact",
  active = list(
    data = return_data
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
                 has_data = FALSE,
                 has_fwd = FALSE,
                 has_up = FALSE,
                 has_right = FALSE,
                 has_lying = FALSE,
                 has_side = FALSE,
                 has_activity = FALSE,
                 sampInt = NA,
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
