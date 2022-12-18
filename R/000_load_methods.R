# ------------------------------------------------------------------------------
# load_... methods of Triact class
# ------------------------------------------------------------------------------


# determine sampling interval in sec

### --> here work needs to be done; helpful message should be raised in case of problems

determine_sampInt <- function(tbl) {
   sInt_by_id <-
      tbl[, .(sInt = unique(difftime(time[-1], time[-length(time)],units = "secs"))), by = id]

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


# Idea for potential direct use in DT[i, j, by = id]

determine_sampInt_NEU <- function(t_vect, intv_grid = 1 / (1:1000)) {

  sInt = unique(difftime(t_vect[-1], t_vect[-length(t_vect)], units = "secs"))

  if (length(sInt) > 1) {

    sInt <- vapply(sInt,
                   FUN = \(sInt) intv_grid[which.min(abs(intv_grid - sInt))],
                   FUN.VALUE = numeric(1))
  }

  return(as.difftime(unique(sInt), units = "secs"))

}


# ----------------------------------------------------------------

# source: https://adv-r.hadley.nz/conditions.html
stop_custom <- function(.subclass, message, call = NULL, ...) {
   err <- structure(list(message = message,
                         call = call,
                         ...),
                    class = c(.subclass, "error", "condition"))
   stop(err)
}

################################################################################

load_files <- function(input,
                       id_substring,
                       timeFwdUpRight_cols = c(1, 2, 3, 4),
                       time_format  = NULL,
                       tz           = Sys.timezone(),
                       start_time   = NULL,
                       end_time     = NULL,
                       sep          = "auto",
                       skip         = "__auto__",
                       parallel     = 1,
                       ...) {

  # check arguments ------------------------------------------------------------

  assertColl <- checkmate::makeAssertCollection()

  ## check input
  if (checkmate::testDirectory(input[1])) {
    checkmate::assertDirectory(input,
                               access = "r",
                               add = assertColl)
  } else if (checkmate::testFile(input[1])) {
    checkmate::assertFile(input,
                          access = "r",
                          extension = c("csv", "tsv", "txt"),
                          add = assertColl)
  } else {
    assertColl$push("'input' not found, must be directory or file names. See ?Triact")
  }

  ## check id_substring
  if (checkmate::testNumeric(id_substring)) {
    msg <- checkmate::checkIntegerish(id_substring,
                                      len = 2,
                                      sorted = TRUE,
                                      any.missing = FALSE,
                                      lower = 1)
    if (!isTRUE(msg)) {
      assertColl$push(paste0(
        "Variable 'id_substring' as integer c(first, last): ", msg))
      }
  } else if (checkmate::testCharacter(id_substring)) {
    msg <- checkmate::checkCharacter(id_substring,
                                     len = 1,
                                     min.chars = 1,
                                     all.missing = FALSE)
    if (!isTRUE(msg)) {
      assertColl$push(paste0(
        "Variable 'id_substring' as character (Perl-like regex): ", msg))
    }
  } else {
    assertColl$push("Variable 'id_substring': Misspecified. See ?Triact")
  }

  ## check timeFwdUpRight_cols
  msg <- checkmate::checkIntegerish(timeFwdUpRight_cols,
                                    len = 4, lower = 1)

  if (!isTRUE(msg)) {
    assertColl$push(paste0("Variable 'timeFwdUpRight_cols': ", msg))
  } else {
    if (is.na(timeFwdUpRight_cols[1])) {
    assertColl$push("Variable 'timeFwdUpRight_cols': First element (time column) cannot be NA.")
    }
    if (checkmate::allMissing(timeFwdUpRight_cols[2:4])) {
      assertColl$push("Variable 'timeFwdUpRight_cols': At least one of the acceleration columns must be non-NA.")
    }
  }

  ## check time_format
  try_err <-
    suppressWarnings(try(lubridate::parse_date_time("_", orders = time_format),
                         silent = TRUE))

  if (!checkmate::testCharacter(time_format,
                                min.len = 1,
                                null.ok = TRUE,
                                any.missing = FALSE) ||
      (inherits(try_err, "try-error") &&
      grepl("Unknown formats supplied", attr(try_err, "condition")$message))) {
    assertColl$push("Variable 'time_format': Unknown format(s) supplied.")
  }

  ## check tz
  if (inherits(try(lubridate::force_tz(Sys.time(), tz), silent = TRUE),
              "try-error")) {
    assertColl$push(paste0("Variable 'tz': Unrecognized time zone"))
  }

  ## check start_time & stop_time
  for (tp in c("start_time", "end_time")) {
    if (!is.null(get(tp))) {
      if (inherits(try(as.POSIXct(get(tp)), silent = TRUE), "try-error") ||
          !checkmate::testPOSIXct(as.POSIXct(get(tp)),
                                  len = 1,
                                  any.missing = FALSE)) {
        assertColl$push(paste0(
          "Variable '", tp, "': Not interpretable as POSIXct of length 1"))
      }
    }
  }

  ## check sep
  checkmate::assertCharacter(sep,
                             len = 1,
                             all.missing = FALSE,
                             add = assertColl)

  ## check skip
  if (!checkmate::testCharacter(skip,
                               len = 1,
                               all.missing = FALSE) &&
      !checkmate::testInt(skip,
                          na.ok = FALSE)) {
    assertColl$push("Variable 'skip': Must be integer or character of length 1")
  }

  ## check parallel
  checkmate::assertInt(parallel,
                       lower = 1,
                       add = assertColl)

  checkmate::reportAssertions(assertColl)

  # prepare for reading  -------------------------------------------------------

  # list files if input is dir
  if (dir.exists(input[1])) {
    input <- list.files(input, full.names = TRUE, recursive = TRUE)
  }

  # extracts ID from file names
  if (is.character(id_substring)) {
    names(input) <-
      sapply(regmatches(
        basename(input),
        regexec(id_substring, basename(input), perl = TRUE)
      ), \(i) i[1])
  } else if (is.numeric(id_substring)) {
    names(input) <-
      substring(basename(input), id_substring[1], id_substring[2])
  }

  if (checkmate::anyMissing(names(input))) {
   stop("Missspecified 'id_substring'. Could not extract id from files: ",
        paste(basename(input[is.na(names(input))]), collapse = ", "),
        call. = FALSE)
  }

  ## extract math sign of accel cols & strip it from timeFwdUpRight_cols
  acc_col_sign <- na.omit(sign(timeFwdUpRight_cols[-1]))
  timeFwdUpRight_cols <- abs(timeFwdUpRight_cols)

  ## prepare col names
  colnms <-
    c("time", "acc_fwd", "acc_up", "acc_right")[!is.na(timeFwdUpRight_cols)]

  ## prepare col classes
  colcls <- as.list(na.omit(timeFwdUpRight_cols))
  if (is.null(time_format)) {
    names(colcls) <- c("POSIXct", rep("numeric", length(colcls) - 1))
  } else {
    names(colcls) <- c("character", rep("numeric", length(colcls) - 1))
  }

  # read files & concatenate ---------------------------------------------------

  read_file <- function(f, fread_args, tformat = time_format) {
    file_dt <- do.call(data.table::fread, c(list(file = f), fread_args))

    if (!is.null(tformat)) {
      file_dt[, time := lubridate::parse_date_time(
        time,
        orders = tformat,
        tz = tz,
        exact = TRUE,
        quiet = TRUE
      )]
    }

    if ((!lubridate::is.POSIXct(file_dt$time)) ||
        checkmate::anyMissing(file_dt$time)) {
      stop_custom("time_parse_error", basename(f))
    }
    return(file_dt)
  }

  arguments <- c(
    list(
      select = timeFwdUpRight_cols,
      tz = if (tz == "UTC")
        "UTC"
      else
        "",
      # fread only takes "UTC" or "" (system tz) --> extra step below needed
      col.names = colnms,
      colClasses = colcls,
      sep = sep,
      skip = skip,
      header = FALSE
    ),
    list(...)
  )

  ## Note on parallelization: when parallel > 1 files are read in parallel via parLapply
  ## in this case nThread in data.table::fread is set to zero to avoid nested parallelization
  ## this behavior can be overwritten by passing nTread via the ... argument

  if (parallel > 1) {
    if (is.null(arguments$nThread)) {
      arguments$nThread <- 1
    }
    fread_cls <- parallel::makeCluster(parallel)
    on.exit(parallel::stopCluster(fread_cls))
    dataList <- parallel::parLapply(cl = fread_cls,
                                    X = input,
                                    fun = \(f) {
                                      tryCatch(
                                        expr = read_file(f, arguments),
                                        time_parse_error = identity
                                      )
                                    })
  } else {
    dataList <- lapply(X = input,
                       FUN = \(f) tryCatch(
                         expr = read_file(f, arguments),
                         time_parse_error = identity
                       ))
  }

  tErrMsg <- unlist(dataList[vapply(dataList,
                                    FUN = is,
                                    FUN.VALUE = logical(1),
                                    class2 = "time_parse_error")])
  if (!is.null(tErrMsg)) {
    stop(paste0(
      "Problem coersing time column to POSIXct for files: ",
      paste(tErrMsg, collapse = ", ")
    ))
  }

  private$dataDT <- data.table::rbindlist(dataList, idcol = "id")

  # tidy, clean etc. -----------------------------------------------------------

  ## Negation of the acceleration data (opposing direction)
  ## according to the math sign specified via the timeFwdUpRight_cols argument

  expr <- str2lang(paste0("list(" ,
                          paste(
                            ifelse(acc_col_sign == -1,
                                   paste0("-", colnms[-1]), colnms[-1]),
                            collapse = ", "
                          ),
                          ")"))

  private$dataDT[, colnms[-1] := eval(expr)]

  ## id to factor
  private$dataDT[, id := as.factor(id)]

  ## drop incomplete
  private$dataDT <-
    private$dataDT[complete.cases(private$dataDT),]

  ## drop duplicates
  private$dataDT <- private$dataDT[!duplicated(private$dataDT),]

  ## set timezone
  attr(private$dataDT$time, "tzone") <- tz

  ## filter time range according to user-provided start and/or end time
  if (!is.null(start_time) && is.null(end_time)) {
    private$dataDT <-
      private$dataDT[time >= as.POSIXct(start_time, tz = tz),]
  } else if (is.null(start_time) && !is.null(end_time)) {
    private$dataDT <-
      private$dataDT[time <= as.POSIXct(end_time, tz = tz)]
  } else if (!is.null(start_time) && !is.null(end_time)) {
    private$dataDT <-
      private$dataDT[(time >= as.POSIXct(start_time, tz = tz)) &
                       (time <= as.POSIXct(end_time, tz = tz))]
  }

  ## drop unused levels in id & warn user

  old_levels <- private$dataDT[, levels(id)]

  private$dataDT[, id := droplevels(id)]

  dropped_levels <- setdiff(old_levels, levels(private$dataDT$id))

  if (length(dropped_levels) > 0) {
    warning(paste0("Filtering using the user-provided start_time/end_time ",
                    "resulted in no data for ids: ",
                    paste(dropped_levels, collapse = ", ")),
            call. = FALSE)
  }

  if (nrow(private$dataDT) == 0) {

    # reset triact object if no data left
    name_of_triact_obtect <- as.character(sys.calls()[[1]][[1]])[2]
    assign(name_of_triact_obtect, Triact$new(), inherits = TRUE)

    stop("No data after filtering with user-provided start_time/end_time left.",
         call. = FALSE)
  }

  # determine sampling interval ------------------------------------------------

  private$sampInt <- determine_sampInt(private$dataDT)

  # return ---------------------------------------------------------------------

  return(invisible(self))
}

################################################################################

load_table <- function(table) {

  # check arguments ------------------------------------------------------------

  assertColl <- checkmate::makeAssertCollection()

  checkmate::assertDataFrame(table, min.rows = 2)

  checkmate::assertNames(colnames(table),
                         what = "colnames",
                         type = "strict",
                         must.include = c("id", "time"),
                         subset.of = c("id", "time", "acc_fwd",
                                       "acc_up", "acc_right"),
                         add = assertColl)

  checkmate::assertSubset(grep("^acc_fwd$|^acc_up$|^acc_right$", colnames(table),
                               value = TRUE, perl = TRUE),
                          choices = c("acc_fwd", "acc_up", "acc_right"),
                          empty.ok = FALSE,
                          .var.name = "'acc_...' columns",
                          add = assertColl)

  checkmate::assertFactor(table$id,
                          empty.levels.ok = FALSE,
                          null.ok = TRUE, # is already tested above
                          any.missing = FALSE,
                          .var.name = "'id' column",
                          add = assertColl)

  checkmate::assertPOSIXct(table$time,
                           any.missing = FALSE,
                           null.ok = TRUE, # is already tested above
                           .var.name = "'time' column",
                           add = assertColl)

  for (col in c("acc_up", "acc_fwd", "acc_right")) {

    checkmate::assertNumeric(table[[col]],
                             any.missing = FALSE,
                             null.ok = TRUE,
                             .var.name = paste0("'", acc_up, "' column"),
                             add = assertColl)
  }

  checkmate::reportAssertions(assertColl)

  # assign data to private$dataDT and determine sampling interval

  private$dataDT <- as.data.table(table)

  private$sampInt <- determine_sampInt(private$dataDT)

  # return ---------------------------------------------------------------------

  return(invisible(self))

}
