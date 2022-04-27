
Triact <- R6::R6Class("Triact",
  active = list(
    data = return_data
  ),
  public = list(
    load_data = load_data,
    add_lying = add_lying,
    add_side = add_side,
    add_activity = add_activity,
    extract_liedown = extract_liedown,
    extract_standup = extract_standup,
    analyze_itervals = analyze_itervals,
    analyze_bouts = analyze_bouts
  ),
  private = list(dataDT = NULL,
                 has_data = NULL,
                 has_fwd = FALSE,
                 has_up = FALSE,
                 has_right = FALSE,
                 has_lying = FALSE,
                 has_side = FALSE,
                 has_activity = FALSE
  )
)
