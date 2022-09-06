# ----------------------------------------------------------------
# add_... methods of Triact class
# ----------------------------------------------------------------

add_lying <- function(crit_lie = 0.5, window_size = 120, check = TRUE) {
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_up, .var.name = "has upward acceleration?")
  checkmate::assertNumber(crit_lie)
  checkmate::assertNumber(window_size, lower = 0, finite = TRUE)
  checkmate::assertFlag(check)

  # determine k
  k <- round(window_size / as.numeric(private$sampInt, units = "secs"), digits = 0)
  k <- if ((k %% 2) == 0) k + 1 else k

  private$dataDT[, lying := as.logical(runmed(acc_up < crit_lie, k, endrule = "constant")), id]
  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]
  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE
  return(invisible(self))
}

# -----------------------------------------

add_lying4 <- function(crit_lie = 0.5,
                       window_size = 10,
                       min_duration_lying = 60,
                       max_median_activity_lying = 1.2, # needs a better name!
                       min_duration_standing = NULL) {

  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_up, .var.name = "has upward acceleration?")
  checkmate::assertNumber(crit_lie)
  checkmate::assertNumber(window_size, lower = 0, finite = TRUE)

  # determine k
  k <- round(window_size / as.numeric(private$sampInt, units = "secs"), digits = 0)
  k <- if ((k %% 2) == 0) k + 1 else k

  private$dataDT[, lying := as.logical(runmed(acc_up < crit_lie, k, endrule = "constant")), id]

  if (!is.null(min_duration_lying) || !is.null(max_median_activity_lying)) {
    private$dataDT[, lying := if (lying[1] && difftime(time[.N], time[1], units = "secs") < min_duration_lying) FALSE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
    private$dataDT[, lying := if (lying[1] && ((difftime(time[.N], time[1], units = "secs") < min_duration_lying) ||
                                               (median(abs(diff(acc_up))) > max_median_activity_lying))
                                 ) FALSE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }

  if (!is.null(min_duration_standing)) {
    private$dataDT[, lying := if (!lying[1] & difftime(time[.N], time[1], units = "secs") < min_duration_standing) TRUE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }

  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]

  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE
  return(invisible(self))
}

# -----------------------------------------

add_lying3 <- function(crit_lie = 0.5,
                       window_size = 10,
                       min_duration_lying = 30,
                       min_duration_standing = NULL) {

  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_up, .var.name = "has upward acceleration?")
  checkmate::assertNumber(crit_lie)
  checkmate::assertNumber(window_size, lower = 0, finite = TRUE)

  # determine k
  k <- round(window_size / as.numeric(private$sampInt, units = "secs"), digits = 0)
  k <- if ((k %% 2) == 0) k + 1 else k

  private$dataDT[, lying := as.logical(runmed(acc_up < crit_lie, k, endrule = "constant")), id]

  if (!is.null(min_duration_lying)) {
    private$dataDT[, lying := if (lying[1] && difftime(time[.N], time[1], units = "secs") < min_duration_lying) FALSE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]

  }

  if (!is.null(min_duration_standing)) {
    private$dataDT[, lying := if (!lying[1] & difftime(time[.N], time[1], units = "secs") < min_duration_standing) TRUE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }

  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]

  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE
  return(invisible(self))
}
# ----------------------------------------------------------------

# -----------------------------------------

add_lying_butter <- function(filter_method = "median",
                             crit_lie = 0.5,
                             minimum_duration_lying = 10,
                             minimum_duration_standing,
                             minimum_duration_lying = 30,
                             minimum_duration_standing = NULL,
                             add_filtered = FALSE,
                             ...) {

  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_up, .var.name = "has upward acceleration?")
  checkmate::assertNumber(crit_lie)
  #checkmate::assertNumber(cutoff, lower = 0, finite = TRUE)

  # list filter method's default values

  filterArgs_defaults <- list()

  filterArgs_defaults[["median"]] = list(window_size = 10)

  filterArgs_defaults[["butter"]] = list(cutoff = 0.01,
                                         order = 1)

  # get args for filter method passed via ... and complete with default values

  fArgs <- list(...)

  for (a in names(filterArgs_defaults[[filter_method]])) {
    if (is.null(fArgs[[a]])) {
      fArgs[a] <- filterArgs_defaults[[filter_method]][a]
    }
  }

  print(fArgs)

  ## Step 1: filtering signal

  if (filter_method == "median") {

    # determine k
    k <- round(fArgs$window_size / as.numeric(private$sampInt, units = "secs"),
               digits = 0)
    k <- if ((k %% 2) == 0) k + 1 else k

    private$dataDT[, acc_up_filtered := runmed(acc_up, k, endrule = "constant"), id]

  } else if (filter_method == "butter") {

    # determine Nyquist freq
    nyq = 0.5 * 1 / as.numeric(private$sampInt, units = "secs")

    normal_cutoff = fArgs$cutoff / nyq # freqs normalized to [0,1], where 1 is nyq

    # define butterworth low-pass filter
    bf <- signal::butter(fArgs$order, normal_cutoff, type = "low", plane = "z")

    # wrapper around signal::filtfilt() that deals with artifacts at end/start
    # vector to be filtered is padded with the reverse vector
    fit_butter <- function(filter, x, max_n_pad) {
      np <- if (length(x) < max_n_pad) length(x) else max_n_pad
      return(signal::filtfilt(bf, c(rev(x[1:np]), x, rev(x)[1:np]))[(np + 1):(length(x) + np)])
    }

    n_5min_pad <- round(5 * 60 / as.numeric(private$sampInt, units = "secs"))

    private$dataDT[, acc_up_filtered := fit_butter(filter = bf,
                                          x = acc_up,
                                          max_n_pad = n_5min_pad), id]
  }

  ## Step 2: thresholding (binarization)

  private$dataDT[, lying := acc_up_filtered < crit_lie, id]

  # Step 3: discard bouts shorter than minimum duration

  if (!is.null(minimum_duration_lying)) {
    private$dataDT[, lying := if (lying[1] && difftime(time[.N], time[1], units = "secs") < minimum_duration_lying) FALSE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }

  if (!is.null(minimum_duration_standing)) {
    private$dataDT[, lying := if (!lying[1] & difftime(time[.N], time[1], units = "secs") < minimum_duration_standing) TRUE,
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }


  # number bouts (uniquely per id)

  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]


  ## tidy, update

  # Order columns with lying information
  co <- c("bout_nr", "lying", "acc_up_filtered")
  co_ord <- c(colnames(private$dataDT)[!colnames(private$dataDT) %in% co], co)
  setcolorder(private$dataDT, co_ord)

  # drop/keep filtered data
  if (!add_filtered) {
    private$dataDT[, acc_up_filtered := NULL]
  }

  # drop lying side data if present and warn user
  if(private$has_side) {
   private$dataDT[, side := NULL]
   private$has_side <- FALSE
   warning("Information on lying side removed. Please re-run $add_side().")
  }

  private$has_lying <- TRUE

  return(invisible(self))

  }
# ----------------------------------------------------------------










add_side <- function(left_leg, crit_left = if(left_leg) -0.5 else 0.5){
  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertTRUE(private$has_right, .var.name = "has right-axis acceleration?")
  if (missing(crit_left)) {checkmate::assertFlag(left_leg)}
  checkmate::assertNumber(crit_left)
  if (!missing(crit_left) & !missing(left_leg)) {
    warning("The argument 'left_leg' is ignored as argument 'crit_left' was provided.", call. = FALSE)
  }

  private$dataDT[, side := as.factor(if(!lying[1]) NA else if (median(acc_right < crit_left)) "L" else "R"), by = .(id, bout_nr)]

  private$has_side <- TRUE

  return(invisible(self))

}

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

##################################################################
# EXPERIMENTAL

check_orientation <- function(crit = 0.5) {

  # argument checks ------------------------------------------------------------

  checkmate::assertNumber(crit)

  # check wrong mounting of logger and correct (180° turned in sagittal plane)--
  check <- parse(text = "sum(acc_up > crit) < sum(acc_up < -1 * crit)")
  up_inverted <- private$dataDT[, .(test = eval(check)), id]
  if (any(up_inverted$test)) {

    message("This function checks for a specific type of incorrect mounting of the potentiometer, namely accidental mounting rotated by 180° in the sagittal plane.")

    warning(paste("For the IDs listed below the the logger seem to have been attached rotated:\n\n"
                  , paste(up_inverted$id[up_inverted$test], collapse = ", ")),
            call. = FALSE)
    private$dataDT[, c("acc_up", "acc_fwd") := if(eval(check)) .(-acc_up, -acc_fwd) else .(acc_up, acc_fwd), id]
    warning(paste("For the IDs listed below the upward- and forward-axis were automatically negated (multiplied by -1) because the data appeared to come from a logger that was mounted 180° rotated (see package documentation):\n\n"
                  , paste(up_inverted$id[up_inverted$test], collapse = ", ")),
            call. = FALSE)
  }
}






add_lying2 <- function(method = "simple", check = TRUE, ...) {

  # argument checks ------------------------------------------------------------

  assertColl <- checkmate::makeAssertCollection()

  checkmate::assertChoice(method,
                          choices = c("simple", "double_focus"),
                          add = TRUE)

  # stuff missing...

  checkmate::reportAssertions(assertColl)

  # determine lying (TRUE/FALSE) -----------------------------------------------

  contr <- list(...)

  # ---- method: simple ----

  if (method == "simple") {

    contr_defaults <- list(crit_lie = 0.6,
                           window_size = 120)

    if (is.null(contr$window_size)) contr$window_size <- contr_defaults$window_size
    if (is.null(contr$crit_lie)) contr$crit_lie <- contr_defaults$crit_lie

    # determine k
    k <- round(contr$window_size / as.numeric(private$sampInt, units = "secs"), digits = 0)
    k <- if ((k %% 2) == 0) k + 1 else k

    private$dataDT[, lying := as.logical(runmed(acc_up < contr$crit_lie, k, endrule = "constant")), id]

  # ---- method: double_focus ----

  } else if (method == "double_focus") {

    contr_defaults <- list(crit_lie = 0.6,
                           window_size_long = 120,
                           window_size_short = 10)

    if (is.null(contr$crit_lie)) contr$crit_lie <- contr_defaults$crit_lie
    if (is.null(contr$window_size_long)) contr$window_size_long <- contr_defaults$window_size_long
    if (is.null(contr$window_size_short)) contr$window_size_short <- contr_defaults$window_size_short

    k_long <- round(contr$window_size_long / as.numeric(private$sampInt, units = "secs"), digits = 0)
    k_long <- if ((k_long %% 2) == 0) k_long + 1 else k_long

    k_short <- round(contr$window_size_short / as.numeric(private$sampInt, units = "secs"), digits = 0)
    k_short <- if ((k_short %% 2) == 0) k_short + 1 else k_short

    private$dataDT[, lying := {lying_long  <- as.logical(runmed(acc_up < contr$crit_lie, k_long, endrule = "constant"))
                               lying_short <- as.logical(runmed(acc_up < contr$crit_lie, k_short, endrule = "constant"))
                               lying_long & lying_short}, id]

  }

  # number lying/standing bouts ------------------------------------------------

  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]

  # tidy -----------------------------------------------------------------------

  nco <- ncol(private$dataDT)
  data.table::setcolorder(private$dataDT, c(1:(nco - 2), nco, nco - 1))
  private$has_lying <- TRUE

  return(invisible(self))
}






















