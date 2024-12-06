################################################################################
# Internal (non-exported) functions
################################################################################

# source: https://adv-r.hadley.nz/conditions.html
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(list(message = message,
                        call = call,
                        ...),
                   class = c(.subclass, "error", "condition"))
  stop(err)
}

################################################################################
# transforms a table to data.frame, tibble, or data.table depending
# on the global option 'triact_table'
# used in all methods of the Triact class which return a table

transform_table <- function(x, table_class = getOption("triact_table", default = "data.frame")) {
  checkmate::assertChoice(table_class, choices = c("data.frame", "data.table", "tibble"), .var.name = "Global option triact_table")

  # drop secondary indices  added during certain actions in DTs
  if (is(x,"data.table")) setindex(x, NULL)

  if (table_class == "data.frame") {
    return(as.data.frame(x))
  } else if (table_class == "data.table") {
    return(data.table::as.data.table(x))
  } else if (table_class == "tibble") {
    return(tibble::as_tibble(x))
  }
}

################################################################################

determine_sampInt <- function(tbl, tol = getOption("triact_tolerance", default = 0.5)) {

  # calc sampling intervals (by id!)
  sInt_by_id <-
    tbl[, .(sInt = difftime(time[-1], time[-length(time)], units = "secs")), by = id]

  # calc median and check for inconsistency (by id!)
  sInt_median_by_id <- sInt_by_id[, {
    {medInt <- median(sInt)} # block prepares temp vars
    .(sInt_median = medInt,
      sInt_inconsistent = any(abs(sInt - medInt) > (tol * medInt)))
    }, by = id]

  if (sInt_median_by_id[, any(sInt_inconsistent)]) {
    stop(
      paste("Inconsistency in sampling frequency for the following id(s):",
            sInt_median_by_id[sInt_inconsistent == TRUE, paste(id, collapse = ", ")],
      "\n\n Possible reasons (presumably non-exhaustive): Gaps in data, non-unique ids")
      )
  }

  # determine median across medians for ids
  sampInt <- sInt_median_by_id[, median(sInt_median)]

  # check for id's with 'different' (median) sampling frequency
  sInt_median_by_id[,  sInt_id_different :=
                      (abs(sInt_median - sampInt)) > (tol * sampInt)]

  if (sInt_median_by_id[, any(sInt_id_different)]) {
    stop(
      paste("Sampling frequency not consistent across ids.",
            "\n Candidate outliers are the following id(s): ",
            sInt_median_by_id[sInt_id_different == TRUE, paste(id, collapse = ", ")]
    ))
  }

  # raise warning if low sampling frequency data...
  if (sampInt > 1) {
    warning("The sampling frequency of your accelelrometer data is <1 Hz.
            Please note that the algorithms in triact with default parameter
            values are intended for data with a sampling frequency of >= 1 Hz.
            Therefore, in your case, lower accuracy up to total failure is to be expected.
            You may be able to counteract this to a certain extent by adjusting parameters.",
            call. = FALSE)
  }

  return(sampInt)
}


################################################################################
# (low pass) filtering of acceleration data

filter_acc <- function(filter_method, axes, fArgs, dba = FALSE) {

  # ---- check fArgs arguments ----

  assertColl <- checkmate::makeAssertCollection()

  if (filter_method == "median") {

    checkmate::assertNumber(fArgs$window_size,
                            lower = 0,
                            add = assertColl,
                            .var.name = "window_size")

  } else if (filter_method == "butter") {

    if ("cutoff" %in% names(fArgs)) {
      checkmate::assertNumber(fArgs$cutoff,
                              lower = 0,
                              # Nyquist freq
                              upper = 0.5 * 1 / as.numeric(private$sampInt,
                                                           units = "secs"),
                              add = assertColl,
                              .var.name = "cutoff")
    }

    if ("order" %in% names(fArgs)) {
      checkmate::assertInt(fArgs$order,
                           lower = 1,
                           add = assertColl,
                           .var.name = "order")
    }

  }

  checkmate::reportAssertions(assertColl)

  # -------------------

  axd <- gsub("acc_", "", axes)

  if (filter_method == "median") {

    # determine k
    k <- round(fArgs$window_size / as.numeric(private$sampInt, units = "secs"),
               digits = 0)

    k <- if ((k %% 2) == 0) k + 1 else k

    private$dataDT[, paste0("gravity_", axd) :=
                      lapply(.SD, \(x) runmed(x, k, endrule = "constant")),
                    by = id,
                    .SDcols = axes]

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
      return(signal::filtfilt(bf, c(rev(x[1:np]), x,
                                    rev(x)[1:np]))[(np + 1):(length(x) + np)])
    }

    n_5min_pad <- round(5 * 60 / as.numeric(private$sampInt, units = "secs"))

    private$dataDT[, paste0("gravity_", axd) :=
                      lapply(.SD, \(x) fit_butter(bf, x, n_5min_pad)),
                   by = id,
                   .SDcols = axes]

    }

    if (dba) {

      nax <- length(axes)

      private$dataDT[, paste0("dba_", axd) := .SD[, 1:nax] - .SD[, -(1:nax)],
                     .SDcols = c(axes, paste0("gravity_", axd))]

      private$dataDT[, paste0("gravity_", axd) := NULL]

    }

}

################################################################################
