# ----------------------------------------------------------------
# add_... methods of Triact class
# ----------------------------------------------------------------

add_lying <- function(filter_method = "median",
                             crit_lie = 0.5,
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

  filterArgs_defaults[["butter"]] = list(cutoff = 0.1,
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
