# ------------------------------------------------------------------------------
# add_... methods of Triact class
# ------------------------------------------------------------------------------

################################################################################

add_lying <- function(filter_method = "median",
                      crit_lie = 0.5,
                      minimum_duration_lying = 30,
                      minimum_duration_standing = NULL,
                      add_filtered = FALSE,
                      ...) {

  # check prerequisites --------------------------------------------------------

  if (!private$has("data")) {
    stop("No accelerometer data found.
         Import data using methods $load_files() or $load_table().",
         call. = FALSE)
  }

  if (!private$has("acc_up")) {
    stop("Lying data is missing. You need to call $add_lying() first.",
         call. = FALSE)
  }

  # argument checks ------------------------------------------------------------

  assertColl <- checkmate::makeAssertCollection()

  # ---- check filter_method ----
  checkmate::assertChoice(filter_method,
                          choices = c("median", "butter"),
                          add = assertColl)

  # ---- check crit_lie ----
  checkmate::assertNumber(crit_lie,
                          add = assertColl)

  # ---- check minimum_duration_lying ----
  checkmate::assertNumber(minimum_duration_lying,
                          lower = 0,
                          null.ok = TRUE,
                          add = assertColl)

  # ---- check minimum_duration_standing ----
  checkmate::assertNumber(minimum_duration_standing,
                          lower = 0,
                          null.ok = TRUE,
                          add = assertColl)

  # ---- check add_filtered ----
  checkmate::assertFlag(add_filtered,
                        add = assertColl)


  checkmate::reportAssertions(assertColl)


  # determine lying/standing  and bouts ----------------------------------------

  ## Step 1: filtering signal

  private$filter_acc(filter_method = filter_method,
                     axes = "acc_up",
                     fArg = list(...))

  ## Step 2: thresholding (binarization)

  private$dataDT[, lying := gravity_up < crit_lie, id]

  # Step 3: discard bouts shorter than minimum duration

  if (!is.null(minimum_duration_lying)) {
    private$dataDT[, lying :=
                     if (lying[1] && difftime(time[.N], time[1], units = "secs")
                         < minimum_duration_lying) {
                       FALSE
                       },
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }

  if (!is.null(minimum_duration_standing)) {
    private$dataDT[, lying :=
                     if (!lying[1] & difftime(time[.N], time[1], units = "secs")
                         < minimum_duration_standing) {
                       TRUE
                     },
                   by = .(id, cumsum(c(1, diff(lying) != 0)))]
  }


  # number bouts (uniquely per id)

  private$dataDT[, bout_nr := cumsum(c(1, diff(lying) != 0)), id]


  # Tidy and update ------------------------------------------------------------

  # Order columns with lying information
  co <- c("bout_nr", "lying", "gravity_up")
  co_ord <- c(colnames(private$dataDT)[!colnames(private$dataDT) %in% co], co)
  setcolorder(private$dataDT, co_ord)

  # drop/keep filtered data
  if (!add_filtered) {
    private$dataDT[, gravity_up := NULL]
  }

  # drop lying side data if present and warn user
  if (private$has("side")) {
   private$dataDT[, side := NULL]
   warning("Information on lying side removed. Please re-run $add_side().")
  }

  return(invisible(self))

  }

################################################################################

add_side <- function(left_leg, crit_left = if(left_leg) -0.5 else 0.5) {

  # check prerequisites --------------------------------------------------------

  if (!private$has("lying")) {
    stop("No lying behaviour data found.
         You need to call $add_lying() first.",
         call. = FALSE)
  }

  if (!private$has("acc_right")) {
    stop("No acceleration from 'right' axis found (acc_right)
         This is prerequisite for determining lying side.",
         call. = FALSE)
  }

  # argument checks ------------------------------------------------------------

  if (missing(crit_left)) {
    checkmate::assertFlag(left_leg)
  }

  checkmate::assertNumber(crit_left)

  if (!missing(crit_left) & !missing(left_leg)) {
    warning("The argument 'left_leg' is ignored as
            argument 'crit_left' was provided.", call. = FALSE)
  }

  # determine lying side -------------------------------------------------------

  private$dataDT[, side := as.factor(if(!lying[1]) NA
                                     else if (median(acc_right < crit_left)) "L"
                                     else "R"),
                 by = .(id, bout_nr)]

  # return ---------------------------------------------------------------------

  return(invisible(self))

}


################################################################################

add_activity <- function(dynamic_parameter = c("dba", "jerk"),
                          norm = "L2",
                          filter_method = "median",
                          keep_dynamic_parameter = FALSE,
                          ...) {

  # check prerequisites --------------------------------------------------------

  if (!private$has("data")) {
    stop("No accelerometer data found.
         Import data using methods $load_files() or $load_table().",
         call. = FALSE)
  }

  # check prerequisites -------------------------------------------------------

  # .... missing

  # determine activity  ------------------------------------------------------

  calc_norm <- function(subdt, L) {
    if (L == "L1") {
      rowSums(sapply(subdt, abs))
    } else if (L == "L2") {
      sqrt(rowSums(sapply(subdt, \(x) x^2)))
    }
  }

  axs <- private$has(c("acc_fwd", "acc_up", "acc_right"))

  if ("jerk" %in% dynamic_parameter) {

    private$dataDT[, delta_time := as.numeric(
      c(NA, difftime(time[-1], time[-length(time)], units = "secs"))), by = id]

    private$dataDT[, c("jerk_fwd", "jerk_up", "jerk_right")[axs] :=
                        lapply(.SD, \(x) {c(NA, diff(x)) / delta_time}),
                   by = id,
                   .SDcols = c("acc_fwd", "acc_up", "acc_right")[axs]]

    private$dataDT[, delta_time := NULL]

    for (l in norm) {

      private$dataDT[, paste0(l, "Jerk") := calc_norm(.SD, L = l),
                      .SDcols = c("jerk_fwd", "jerk_up", "jerk_right")[axs]]

    }

    if (!keep_dynamic_parameter) {
      private$dataDT[, c("jerk_fwd", "jerk_up", "jerk_right")[axs] := NULL]
    }
  }

  if ("dba" %in% dynamic_parameter) {

    fArgs <- list(...)

    private$filter_acc(filter_method,
                        axes = c("acc_fwd", "acc_up", "acc_right")[axs],
                        fArgs,
                        dba = TRUE)


    for (l in norm) {

      private$dataDT[, paste0(l, "DBA") := calc_norm(.SD, L = l),
                     .SDcols = c("dba_fwd", "dba_up", "dba_right")[axs]]
    }

    if (!keep_dynamic_parameter) {
      private$dataDT[, c("dba_fwd", "dba_up", "dba_right")[axs] := NULL]
    }

  }


  # Return ---------------------------------------------------------------------

  return(invisible(self))

}









