# ----------------------------------------------------------------
# extract_... methods of Triact class
# ----------------------------------------------------------------

# internal function
extract_updown <- function(self, private, sec_before, sec_after, updown) { # internal

  if (!private$has("lying")) {
    stop("No lying behaviour data found.
         You need to call $add_lying() first.",
         call. = FALSE)
  }

  checkmate::assertNumber(sec_before, lower = 0)
  checkmate::assertNumber(sec_after, lower = 0)

  L <- switch(updown, "down" = FALSE, "up" = TRUE)

  private$dataDT[, switch := data.table::frollapply(lying, 2, \(i) {i[1] == L & i[2] == !L},
                                                     align = if (L) "left" else "right"), by = id, ]

  updown_times <- private$dataDT[as.logical(switch), c("id", "time", "bout_nr", "side")]

  private$dataDT[, switch := NULL]

  if ((sec_before == 0) && (sec_after == 0)) {

    return(transform_table(updown_times))

  } else {

    updown_results <- lapply(1:nrow(updown_times), \(r) {
      private$dataDT[(id == updown_times[[r, "id"]]) & (time >= (updown_times[[r, "time"]] - sec_before) & time <= (updown_times[[r, "time"]] + sec_after))]
    })

    return(lapply(updown_results, transform_table))
  }
}

# ----------------------------------------------------------------

extract_liedown <- function(sec_before = 0, sec_after = 0) {
  return(extract_updown(self, private, sec_before, sec_after, updown = "down"))
}

# ----------------------------------------------------------------

extract_standup <- function(sec_before = 0, sec_after = 0) {
  return(extract_updown(self, private, sec_before, sec_after, updown = "up"))
}




