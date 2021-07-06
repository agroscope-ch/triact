
add_lying <- function(crit_lie = -0.5, k = 121) {
  checkmate::assertNumber(crit_lie)
  checkmate::assertInt(k, lower = 1)
  checkmate::assert(private$has_Y == TRUE, .var.name = "has Y acceleration?")
  self$data[, lying := runmed(accel_Y > crit_lie, k, endrule = "constant"), id]
  self$data[, bout_id := cumsum(c(0, diff(lying) != 0)), id]
  nco <- ncol(self$data)
  data.table::setcolorder(self$data, c(1:(nco - 2), nco, nco - 1))
  return(invisible(self))
}

add_side <- function(crit_left = -0.5) {
  checkmate::assertNumber(crit_left)
  checkmate::assert(private$has_Z == TRUE, .var.name = "has Z acceleration?")
  self$data[, side := if (lying[1] == 0) as.character(NA) else if (median(accel_Z < crit_left)) "L" else "R", by = bout_id]
  return(invisible(self))
}

# -----------------------------------------------------------------------

extract_updown <- function(self, sec_before, sec_after, updown) { # internal
  L1 <- switch(updown, "down" = 0, "up" = 1)
  L2 <- switch(updown, "down" = 1, "up" = 0)
  liedown_times <- self$data[as.logical(data.table::frollapply(lying, 2, function(i) {i[1] == L1 & i[2] == L2})), time, id]
  if (is.null(sec_before) & is.null(sec_after)) {
    return(liedown_times)
  } else {
    liedown_times$id
    return(lapply(1:nrow(liedown_times), function(r) {
      self$data[(id == liedown_times[[r, "id"]]) & (time >= (liedown_times[[r, "time"]] - sec_before) & time <= (liedown_times[[r, "time"]] + sec_after))]
    }))
  }
}

extract_liedown <- function(sec_before = NULL, sec_after = NULL) {
  checkmate::assertNumber(sec_before, lower = 0, null.ok = TRUE)
  checkmate::assertNumber(sec_after, lower = 0, null.ok = TRUE)
  return(extract_updown(self, sec_before, sec_after, updown = "down"))
}

extract_standup <- function(sec_before = NULL, sec_after = NULL) {
  checkmate::checkNumber(sec_before, lower = 0, null.ok = TRUE)
  checkmate::checkNumber(sec_after, lower = 0, finit = TRUE, null.ok = TRUE)
  return(extract_updown(self, sec_before, sec_after, updown = "up"))
}

# -----------------------------------------------------------------------

get_activity_by_iterval <- function(interval = "hour", lag = 0) {
  .SDcols <- c("accel_X", "accel_Y", "accel_Z")[c(private$has_X, private$has_Y, private$has_Z)]
  activity <- self$data[ , .(activity = mean(sapply(.SD, function(x) sum(abs(diff(x)))))),
                        by = .(id, time = lubridate::floor_date(self$data$time - lag, interval) + lag),
                        .SDcols = .SDcols]
  return(activity)
}



