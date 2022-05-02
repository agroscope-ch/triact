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

  private$dataDT[, side := as.factor(if(!lying[1]) NA else if (median(acc_right < crit_left)) "L" else "R"), by = .(id, bout_nr)]

  private$has_side <- TRUE

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

summarize_itervals <- function(interval = "hour", lag_in_s = 0, duration_units = "mins") {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assert_number(lag_in_s, finite = TRUE)
  checkmate::assert_choice(duration_units, c("secs", "mins", "hours"))

  col_calcs <- quote(list(centerTime = minT + (interval_duration / 2),
                       endTime = maxT + private$sampInt,
                       duration = interval_duration,
                       meanActivity = mean(activity, na.rm = TRUE),
                       standingDuration = mean(!lying) * interval_duration,
                       lyingDuration = mean(lying) * interval_duration))

  if (!private$has_activity) col_calcs["meanActivity"] <- NULL
  if (!private$has_lying) col_calcs[c("standingDuration", "lyingDuration")] <- NULL

  analysis <- private$dataDT[ , {{minT = min(time); maxT = max(time)
                                  interval_duration <- difftime(maxT, minT) + private$sampInt
                                  units(interval_duration) <- duration_units
                                  interval_duration <- as.numeric(interval_duration)} # block prepares temp vars
                                 eval(col_calcs)},                                    # this is returned
                              by = .(id, startTime = lubridate::floor_date(time - lag_in_s, interval) + lag_in_s)]

  return(transform_table(analysis))
}

# ----------------------------------------------------------------
# ----------------------------------------------------------------

summarize_bouts <- function(bout_type = "both", duration_units = "mins", calc_for_incomplete = FALSE) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertChoice(bout_type, choices = c("both", "lying", "upright"))
  checkmate::assert_choice(duration_units, c("secs", "mins", "hours"))

  bout_select <- if (bout_type == "both") {
    TRUE
  } else if (bout_type == "lying") {
    private$dataDT$lying
  } else if (bout_type == "upright") {
    !private$dataDT$lying
  }

  col_calcs <- quote(list(startTime = minT,
                       endTime = maxT + private$sampInt,
                       duration  = {interval_duration <- difftime(maxT, minT) + private$sampInt
                                    units(interval_duration) <- duration_units
                                    as.numeric(interval_duration)},
                       meanActivity  = mean(activity, na.rm = TRUE),
                       lying = unique(lying),
                       side = unique(side)))

  if (!private$has_activity) col_calcs["meanActivity"] <- NULL
  if (!private$has_side) col_calcs["side"] <- NULL

  analysis <- private$dataDT[bout_select, {{minT <- min(time); maxT <- max(time)} # block prepares temp vars
                                           eval(col_calcs)},                      # this is returned
                             by = .(id, bout_nr)]

  if (!calc_for_incomplete) {

    analysis[, c("duration", "startTime", "endTime") := .(
                 ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)), NA, duration),
                 ifelse((bout_nr == 1), NA, startTime),
                 ifelse((bout_nr == max(bout_nr)), NA, endTime)),
              by = id]

    if (private$has_activity) {

      analysis[, meanActivity := ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)), NA, meanActivity), by = id]

    }

  }

  return(transform_table(analysis))
}




