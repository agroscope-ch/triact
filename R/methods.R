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
extract_updown <- function(self, private, sec_before, sec_after, updown) { # internal
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertNumber(sec_before, lower = 0)
  checkmate::assertNumber(sec_after, lower = 0)

  L <- switch(updown, "down" = FALSE, "up" = TRUE)
  private$dataDT[, switch := data.table::frollapply(lying, 2, function(i) {i[1] == L & i[2] == !L}), by = id]
  liedown_times <- private$dataDT[as.logical(switch)]
  private$dataDT[, switch := NULL]
  if ((sec_before == 0) & (sec_after == 0)) {
    return(transform_table(liedown_times))
  } else {
    liedown_times$id
    liedown_results <- lapply(1:nrow(liedown_times), function(r) {
      private$dataDT[(id == liedown_times[[r, "id"]]) & (time >= (liedown_times[[r, "time"]] - sec_before) & time <= (liedown_times[[r, "time"]] + sec_after))]
    })
    return(lapply(liedown_results, transform_table))
  }
}

extract_liedown <- function(sec_before = 0, sec_after = 0) {
  return(extract_updown(self, private, sec_before, sec_after, updown = "down"))
}

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

# ----------------------------------------------------------------
# ----------------------------------------------------------------

analyze_itervals <- function(interval = "hour", lag_in_s = 0) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assert_number(lag_in_s, finite = TRUE)
  analysis <- private$dataDT[ , .(activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right)),
                        by = .(id, time = lubridate::floor_date(time - lag_in_s, interval) + lag_in_s),
                        .SDcols = colnames(private$dataDT)] # unclear why defining .SDcols is needed here (bug in data.table?)
  return(transform_table(analysis))
}

# ----------------------------------------------------------------
# ----------------------------------------------------------------

analyze_bouts <- function(bout_type = "all") {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertChoice(bout_type, choices = c("all", "lying", "upright"))

  bout_select <- if (bout_type == "all") {
    TRUE
  } else if (bout_type == "lying") {
    private$dataDT$lying
  } else if (bout_type == "upright") {
    !private$dataDT$lying
  }

  if (private$has_side) {
    col_cals <- expression(.(activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right),
                                     lying = unique(lying),
                                     side = unique(side)))
  } else {
    col_cals <- expression(.(activity = calc_activity(.SD, private$has_fwd, private$has_up, private$has_right),
                                     lying = unique(lying)))
  }

  analysis <- private$dataDT[bout_select, eval(col_cals), by = .(id, bout_nr)]

  return(transform_table(analysis))
}




