
add_lying <- function(crit_lie = -0.5, k = 121) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_Y, .var.name = "has Y acceleration?")
  checkmate::assertNumber(crit_lie)
  checkmate::assertInt(k, lower = 1)
  private$dataDT[, lying := runmed(accel_Y > crit_lie, k, endrule = "constant"), id]
  private$dataDT[, bout_id := cumsum(c(0, diff(lying) != 0)), id]
  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE
  return(invisible(self))
}

add_side <- function(crit_left = -0.5) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertTRUE(private$has_Z, .var.name = "has Z acceleration?")
  checkmate::assertNumber(crit_left)
  private$dataDT[, side := if (lying[1] == 0) as.character(NA) else if (median(accel_Z < crit_left)) "L" else "R", by = bout_id]
  return(invisible(self))
}

# -----------------------------------------------------------------------

extract_updown <- function(self, private, sec_before, sec_after, updown) { # internal
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertNumber(sec_before, lower = 0, null.ok = TRUE)
  checkmate::assertNumber(sec_after, lower = 0, null.ok = TRUE)
  L1 <- switch(updown, "down" = 0, "up" = 1)
  L2 <- switch(updown, "down" = 1, "up" = 0)
  liedown_times <- private$dataDT[as.logical(data.table::frollapply(lying, 2, function(i) {i[1] == L1 & i[2] == L2})), time, id]
  if (is.null(sec_before) & is.null(sec_after)) {
    return(liedown_times)
  } else {
    liedown_times$id
    return(lapply(1:nrow(liedown_times), function(r) {
      private$dataDT[(id == liedown_times[[r, "id"]]) & (time >= (liedown_times[[r, "time"]] - sec_before) & time <= (liedown_times[[r, "time"]] + sec_after))]
    }))
  }
}

extract_liedown <- function(sec_before = NULL, sec_after = NULL) {
  return(extract_updown(self, private, sec_before, sec_after, updown = "down"))
}

extract_standup <- function(sec_before = NULL, sec_after = NULL) {
  return(extract_updown(self, private, sec_before, sec_after, updown = "up"))
}

# -----------------------------------------------------------------------

calc_activity <- function(accel_dt) {
  duration <- as.numeric(difftime(max(accel_dt$time), min(accel_dt$time), units = "hours"))
  return(sum(sapply(accel_dt[, -1], function(x) sum(abs(diff(x))))) / duration)
}

get_activity_by_iterval <- function(interval = "hour", lag_in_s = 0) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assert_number(lag_in_s, finite = TRUE)
  .SDcols <- c("time", "accel_X", "accel_Y", "accel_Z")[c(TRUE, private$has_X, private$has_Y, private$has_Z)]
  activity <- private$dataDT[ , .(activity = calc_activity(.SD)),
                        by = .(id, time = lubridate::floor_date(private$dataDT$time - lag_in_s, interval) + lag_in_s),
                        .SDcols = .SDcols]
  return(activity)
}


get_activity_by_bout <- function(bout_type = "all") {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertChoice(bout_type, choices = c("all", "lying", "upright"))
  .SDcols <- c("time", "accel_X", "accel_Y", "accel_Z")[c(TRUE, private$has_X, private$has_Y, private$has_Z)]
  lie <- if (bout_type == "all") {
            TRUE
         } else if (bout_type == "lying") {
            private$dataDT$lying == 1
         } else if (bout_type == "upright") {
            private$dataDT$lying == 0
         }
  activity <- private$dataDT[lie, .(activity = calc_activity(.SD), lying = unique(lying)),
                        by = .(id, bout_id),
                        .SDcols = .SDcols]
  return(activity)
}




