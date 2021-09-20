#' R6 class for accelerometer data from cows
#'
#' An object for containing and analyzing triaxial accelerometer data from hind legs of cows.
#' Analyses focus on standing and lying behaviour and activity of the cows.
Triact <- R6::R6Class("Triact",
  active = list(
    #' @field data Raw and classified accelerometer data. Modified by $load_data, $add_lying, $add_side methods.
    data = return_data
  ),
  public = list(
    load_data = load_data,
    add_lying = add_lying,
    add_side = add_side,
    extract_liedown = extract_liedown,
    extract_standup = extract_standup,
    get_activity_by_iterval = get_activity_by_iterval,
    get_activity_by_bout = get_activity_by_bout
  ),
  private = list(dataDT = NULL,
                 has_data = NULL,
                 has_X = FALSE,
                 has_Y = FALSE,
                 has_Z = FALSE,
                 has_lying = FALSE
  )
)
