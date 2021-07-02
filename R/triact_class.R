

# Method naming...
# add_XX
# extract_YY
# summarize_ZZ (?)

Triact <- R6::R6Class("Triact",
  public = list(data = NULL,
    load_data = load_data,
    add_lying = add_lying,
    add_side = add_side,
    extract_liedown = extract_liedown,
    extract_standup = extract_standup
    ),
  private = list(has_X = FALSE,
                 has_Y = FALSE,
                 has_Z = FALSE)
)



