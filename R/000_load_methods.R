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

   # argument checks -----------------------------------------------------------

   assertColl <- checkmate::makeAssertCollection()

   # ---- input ----
   if (checkmate::testDirectory(input[1])) {
      checkmate::assertDirectory(input,
                                 access = "r",
                                 add = assertColl)
   } else if (checkmate::testFile(input[1])) {
      checkmate::assertFile(
         input,
         access = "r",
         extension = c("csv", "tsv", "txt"),
         add = assertColl
      )
   } else {
      assertColl$push("'input' not found, must be directory or file names. See ?Triact")
   }

   # ---- id_substring ----
   if (checkmate::testNumeric(id_substring)) {
      msg <- checkmate::checkIntegerish(
         id_substring,
         len = 2,
         sorted = TRUE,
         any.missing = FALSE,
         lower = 1
      )
      if (!isTRUE(msg)) {
         assertColl$push(paste0(
            "Variable 'id_substring' as integer c(first, last): ",
            msg
         ))
      }
   } else if (checkmate::testCharacter(id_substring)) {
      msg <- checkmate::checkCharacter(
         id_substring,
         len = 1,
         min.chars = 1,
         all.missing = FALSE
      )
      if (!isTRUE(msg)) {
         assertColl$push(paste0(
            "Variable 'id_substring' as character (Perl-like regex): ",
            msg
         ))
      }
   } else {
      assertColl$push("'id_substring' misspecified. See ?Triact")
   }

   # ---- timeFwdUpRight_cols ----
   msg <- checkmate::checkIntegerish(
      timeFwdUpRight_cols,
      len = 4,
      lower = 1,
      all.missing = FALSE
   )

   if (!isTRUE(msg)) {
      assertColl$push(paste0("Variable 'timeFwdUpRight_cols': ", msg))
   } else if (is.na(timeFwdUpRight_cols[1])) {
      assertColl$push("Variable 'timeFwdUpRight_cols': First element (time column) cannot be NA.")
   }



   checkmate::reportAssertions(assertColl)


   # new title  ----------------------------------------------------------------


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

   # extract math sign of accel cols to new var and strip it from timeFwdUpRight_cols

   acc_col_sign <- na.omit(sign(timeFwdUpRight_cols[-1]))

   timeFwdUpRight_cols <- abs(timeFwdUpRight_cols)

   # prepare col names
   colnms <-
      c("time", "acc_fwd", "acc_up", "acc_right")[!is.na(timeFwdUpRight_cols)]

   # prepare col classes: when user does not supply time_format then POSIXct for time col, otherwise character
   colcls <- as.list(na.omit(timeFwdUpRight_cols))
   if (is.null(time_format)) {
      names(colcls) <- c("POSIXct", rep("numeric", length(colcls) - 1))
   } else {
      names(colcls) <- c("character", rep("numeric", length(colcls) - 1))
   }

   # -----------------------------------------------------------

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
          (sum(is.na(file_dt$time)) > 0)) {
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

   # Note on parallelization: when parallel > 1 files are read in parallel via parLapply
   # in this case nThread in data.table::fread is set to zero to avoid nested parallelization
   # this behavior can be overwritten by passing nTread via the ... argument

   if (parallel > 1) {
      if (is.null(arguments$nThread)) {
         # to avoid nested parallelization
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

   # ---------------------------
   # Negation of the acceleration data (opposing direction)
   # according to the math sign specified via the timeFwdUpRight_cols argument

   expr <- str2lang(paste0("list(" ,
                           paste(
                              ifelse(acc_col_sign == -1, paste0("-", colnms[-1]), colnms[-1]),
                              collapse = ", "
                           ),
                           ")"))

   private$dataDT[, colnms[-1] := eval(expr)]

   # ---------------------------
   private$dataDT[, id := as.factor(id)]

   private$dataDT <-
      private$dataDT[complete.cases(private$dataDT),]

   private$dataDT <- private$dataDT[!duplicated(private$dataDT),]

   attr(private$dataDT$time, "tzone") <- tz

   # filter time range according to user-provided start and/or end time
   if (!is.null(start_time) && is.null(start_time)) {
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

   # --------------------------

   private$sampInt <- determine_sampInt(private$dataDT)

   return(invisible(self))
}

# ----------------------------------------------------------------

load_table <- function(table) {

  private$dataDT <- as.data.table(table)
  private$sampInt <- determine_sampInt(private$dataDT)

}
