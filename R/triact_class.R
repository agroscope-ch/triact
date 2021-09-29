
Triact <- R6::R6Class("Triact",
  active = list(
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
