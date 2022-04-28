load_data <- function(input,
                      id_substring,
                      start_time   = NULL,
                      end_time     = NULL,
                      timeFwdUpRight_cols = c(1, 2, 3, 4),
                      time_format  = NULL,
                      tz           = Sys.timezone(),
                      sep          = "auto",
                      skip         = "__auto__",
                      parallel     = 1,
                      ...) {

   # list files if input is dir
   if (dir.exists(input)) {
      input <- list.files(input, full.names = TRUE)
   }

   # extracts ID from file names
   if (is.character(id_substring)) {
      names(input) <- sapply(regmatches(basename(input), regexec(id_substring, basename(input))), function(i) i[2])
   } else if (is.numeric(id_substring)) {
      names(input) <- substring(basename(input), id_substring[1], id_substring[2])
   }

   # prepare col names
   colnms <- c("time", "acc_fwd", "acc_up", "acc_right")[!is.na(timeFwdUpRight_cols)]

   # prepare col classes: when user does not supply time_format then POSIXct for time col, otherwise character
   colcls <- as.list(na.omit(timeFwdUpRight_cols))
   if (is.null(time_format)) {
      names(colcls) <- c("POSIXct", rep("numeric", length(colcls) - 1))
   } else {
      names(colcls) <- c("character", rep("numeric", length(colcls) - 1))
   }

   # -----------------------------------------------------------

   arguments <- c(list(
                  select = timeFwdUpRight_cols,
                  tz = if (tz == "UTC") "UTC" else "", # fread only takes "UTC" or "" (system tz) --> extra step below needed
                  col.names = colnms,
                  colClasses = colcls,
                  sep = sep,
                  skip = skip,
                  header = FALSE), list(...))

   read_file <- function(f, arguments) {
      do.call(data.table::fread, c(list(file = f), arguments))
   }

   # Note on parallelization: when parallel > 1 files are read in parallel via parLapply
   # in this case nThread in data.table::fread is set to zero to avoid nested parallelization
   # this behavior can be overwritten by passing nTread via the ... argument

   if (parallel > 1) {
      if (is.null(arguments$nThread)) arguments$nThread <- 1 # to avoid nested parallelization
      fread_cls <- parallel::makeCluster(parallel)
      on.exit(parallel::stopCluster(fread_cls))
      private$dataDT <- data.table::rbindlist(parallel::parLapply(cl = fread_cls, input, read_file, arguments), idcol = "id")
   } else {
      private$dataDT <- data.table::rbindlist(lapply(input, read_file, arguments), idcol = "id")
   }

   # ---------------------------

   private$dataDT[, id := as.factor(id)]

   private$dataDT <- private$dataDT[complete.cases(private$dataDT), ]

   private$dataDT <- private$dataDT[!duplicated(private$dataDT), ]

   attr(private$dataDT$time, "tzone") <- tz

   # conversion to POSIX in case of user supplied time_format
   if (class(private$dataDT$time)[1] == "character") {
      private$dataDT[, time := as.POSIXct(time, format = time_format, tz = tz)]
   }

   # filter time range according to user-provided start and/or end time
   if (!is.null(start_time) && is.null(start_time)) {
      private$dataDT <- private$dataDT[time >= as.POSIXct(start_time, tz = tz), ]
   } else if (is.null(start_time) && !is.null(end_time)) {
      private$dataDT <- private$dataDT[time <= as.POSIXct(end_time, tz = tz)]
   } else if (!is.null(start_time) && !is.null(end_time)) {
      private$dataDT <- private$dataDT[(time >= as.POSIXct(start_time, tz = tz)) & (time <= as.POSIXct(end_time, tz = tz))]
   }

   # --------------------------

   # determine sampling interval in sec

   ### --> here work needs to be done; helpful message should be raised in case of problems

   sInt_by_id <- private$dataDT[, .(sInt = unique(difftime(time[-1], time[-length(time)], units = "secs"))), by = id]

   freq_grid = 1 / c(1:1000)

   round_to_freq_interv <- function(x) {sapply(x, function(x) freq_grid[which.min(abs(freq_grid - x))])}

   sInt <- sInt_by_id[, round_to_freq_interv(sInt)]

   private$sampInt <- as.difftime(unique(sInt), units = "secs")

   # --------------------------

   # note availability of acceleration directions (for checks by other methods)
   private$has_data <- checkmate::checkDataTable(private$dataDT)
   private$has_fwd <- "acc_fwd" %in% colnames(private$dataDT)
   private$has_up <- "acc_up" %in% colnames(private$dataDT)
   private$has_right <- "acc_right" %in% colnames(private$dataDT)

   return(invisible(self))
}

