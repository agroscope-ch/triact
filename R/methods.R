# ----------------------------------------------------------------
# ----------------------------------------------------------------
add_lying <- function(crit_lie = 0.5, k = 121, check = TRUE) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_up, .var.name = "has upward acceleration?")
  checkmate::assertNumber(crit_lie)
  checkmate::assertInt(k, lower = 1)
  checkmate::assertFlag(check)

  # Check for correct mounting of logger
  if (check) {
    check <- parse(text = "sum(acc_up > crit_lie) < sum(acc_up < -1 * crit_lie)")
    up_inverted <- private$dataDT[, .(test = eval(check)), id]
    if (any(up_inverted$test)) {
      private$dataDT[, c("acc_up", "acc_fwd") := if(eval(check)) .(-acc_up, -acc_fwd) else .(acc_up, acc_fwd), id]
      warning(paste("For the IDs listed below the upward- and forward-axis were automatically negated (multiplied by -1) because the data appeared to come from a logger that was mounted 180° rotated (see package documentation):\n\n"
                    , paste(up_inverted$id[up_inverted$test], collapse = ", ")),
              call. = FALSE)
    }
  }

  private$dataDT[, lying := as.logical(runmed(acc_up < crit_lie, k, endrule = "constant")), id]
  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]
  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE
  return(invisible(self))
}

# ----------------------------------------------------------------
# ----------------------------------------------------------------
add_side <- function(crit_left = -0.5) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertTRUE(private$has_right, .var.name = "has right-axis acceleration?")
  checkmate::assertNumber(crit_left)

  private$dataDT[, side := as.factor(if (lying[1] == 0) NA else if (median(acc_right < crit_left)) "L" else "R"), by = bout_nr]

  private$has_side <- TRUE

  return(invisible(self))

}

# ----------------------------------------------------------------
# ----------------------------------------------------------------

add_activity <- function(add_jerk = FALSE) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")

  private$dataDT[, delta_time := as.numeric(c(NA, difftime(time[-1], time[-length(time)], units = "secs"))), by = id]

  axis <- c(private$has_fwd, private$has_up, private$has_right)

  private$dataDT[, c("jerk_fwd", "jerk_up", "jerk_right")[axis] := lapply(.SD, function(x) {c(NA, diff(x)) / delta_time}),
                 .SDcols = c("acc_fwd", "acc_up", "acc_right")[axis]]

  private$dataDT[, delta_time := NULL]

  private$dataDT[, activity := sqrt(rowSums(sapply(.SD, function(x) x^2))),
                 .SDcols = c("jerk_fwd", "jerk_up", "jerk_right")[axis]]

  if (!add_jerk) {

    private$dataDT[, c("jerk_fwd", "jerk_up", "jerk_right")[axis] := NULL]

  }

  private$has_activity <- TRUE

  return(invisible(self))

}

# ----------------------------------------------------------------
# ----------------------------------------------------------------
extract_updown <- function(self, private, sec_before, sec_after, updown) { # internal
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertNumber(sec_before, lower = 0)
  checkmate::assertNumber(sec_after, lower = 0)

  L <- switch(updown, "down" = FALSE, "up" = TRUE)

  private$dataDT[, switch := data.table::frollapply(lying, 2, function(i) {i[1] == L & i[2] == !L},
                                                    align = if (L) "left" else "right"), by = id, ]

  updown_times <- private$dataDT[as.logical(switch), c("id", "time", "bout_nr", "side")]

  private$dataDT[, switch := NULL]

  if ((sec_before == 0) && (sec_after == 0)) {

    return(transform_table(updown_times))

  } else {

    updown_results <- lapply(1:nrow(updown_times), function(r) {
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

# ----------------------------------------------------------------
# ----------------------------------------------------------------

calc_activity <- function(dataDT, use_fwd, use_up, use_right) {
  activity = dataDT[, mean(sqrt(c(if (use_fwd) (diff(acc_fwd) / as.numeric(diff(time)))^2,
                                  if (use_up) (diff(acc_up) / as.numeric(diff(time)))^2,
                                  if (use_right) (diff(acc_right) / as.numeric(diff(time)))^2)))]
  return(activity)
}

# calc_lyingDuration <- function(dataDT) {
#   lyingDuration = dataDT[, mean(lying) * difftime(max(time) - min(time), units = )]
#   return(lyingDuration)
# }


# ----------------------------------------------------------------
# ----------------------------------------------------------------

analyze_itervals <- function(interval = "hour", lag_in_s = 0, duration_units = "mins") {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assert_number(lag_in_s, finite = TRUE)

  if (private$has_lying) {

    duration <- function(x, units = duration_units) {difftime(max(x), min(x), units = units)}

    col_calcs <- expression(.(centerTime = min(time) + (duration(time) / 2),
                              endTime = max(time),
                              # totalDuration = duration(time),
                              activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right),
                              standingDuration = mean(!lying) * duration(time),
                              lyingDuration = mean(lying) * duration(time)))
  } else {

    col_calcs <- expression(.(centerTime = min(time) + diff(range(time)),
                              endTime = max(time),
                              activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right)))

  }

  analysis <- private$dataDT[ , eval(col_calcs),
                             by = .(id, startTime = lubridate::floor_date(time - lag_in_s, interval) + lag_in_s),
                             .SDcols = colnames(private$dataDT)] # unclear why defining .SDcols is needed here (bug in data.table?)
  return(transform_table(analysis))
}

# ----------------------------------------------------------------
# ----------------------------------------------------------------

analyze_bouts <- function(bout_type = "both", duration_units = "mins") {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertChoice(bout_type, choices = c("both", "lying", "upright"))

  bout_select <- if (bout_type == "both") {
    TRUE
  } else if (bout_type == "lying") {
    private$dataDT$lying
  } else if (bout_type == "upright") {
    !private$dataDT$lying
  }

  if (private$has_side) {
    col_calcs <- expression(.(startTime = min(time),
                              endTime = max(time),
                              duration  = difftime(max(time), min(time), units = duration_units),
                              activity  = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right),
                              lying = unique(lying),
                              side = unique(side)))
  } else {
    col_calcs <- expression(.(startTime = min(time),
                              endTime = max(time),
                              duration  = difftime(max(time), min(time), units = duration_unit),
                              activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right),
                              lying = unique(lying)))
  }

  analysis <- private$dataDT[bout_select, eval(col_calcs), by = .(id, bout_nr)]

  return(transform_table(analysis))
}




