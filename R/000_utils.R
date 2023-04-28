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
  if (table_class == "data.frame") {
    return(as.data.frame(x))
  } else if (table_class == "data.table") {
    return(data.table::as.data.table(x))
  } else if (table_class == "tibble") {
    return(tibble::as_tibble(x))
  }
}

################################################################################
# determined accelerometer sampling frequency

determine_sampInt <- function(tbl) {
  sInt_by_id <-
    tbl[, .(sInt = unique(difftime(time[-1], time[-length(time)], units = "secs"))), by = id]

  freq_grid = 1 / (1:1000)

  round_to_freq_interv <-
    function(x) {
      sapply(x, \(x) freq_grid[which.min(abs(freq_grid - x))])
    }

  sInt <- unique(sInt_by_id[, round_to_freq_interv(sInt)])

  checkmate::assertNumber(
    sInt,
    finite = TRUE,
    lower = 0,
    null.ok = FALSE,
    na.ok = FALSE,
    .var.name = "PROBLEM WITH YOUR SAMPLING FREQ - Maebe you use data with
        differrent freqs? Or your cow id is not unique?..."
  )

  return(as.difftime(sInt, units = "secs"))

}

################################################################################
# (low pass) filtering of acceleration data

filter_acc <- function(filter_method, axes, fArgs, dba = FALSE) {

  fArgsDef <- list()

  if (filter_method == "median") {

    fArgsDef <- list(window_size = 5) # Defaults for "median"

  } else if (filter_method == "butter") {

    fArgsDef <- list(cutoff = 0.1, # Defaults for "butter"
                     order = 1)
  }

  fArgs <- c(fArgs, fArgsDef[!names(fArgsDef) %in% names(fArgs)])

  # -------------------

  # ---- check fArgs arguments ----

  assertColl <- checkmate::makeAssertCollection()

  if (filter_method == "median" && (length(fArgs) > 0)) {

    checkmate::assertNames(names(fArgs),
                           type = "unique",
                           subset.of = names(fArgsDef),
                           add = assertColl,
                           what = "arguments for filter_method 'median'",
                           .var.name = "...")

    checkmate::assertNumber(fArgs$window_size,
                            lower = 0,
                            add = assertColl,
                            .var.name = "window_size")

  } else if (filter_method == "butter" && (length(fArgs) > 0)) {

    checkmate::assertNames(names(fArgs),
                           type = "unique",
                           subset.of = names(fArgsDef),
                           add = assertColl,
                           what = "arguments for filter_method 'butter'",
                           .var.name = "...")

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
