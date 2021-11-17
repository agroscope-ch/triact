load_data <- function(input,
                      id_substring,
                      start_time   = NULL,
                      end_time     = NULL,
                      timeXYZ_cols = c(1, 2, 3, 4),
                      time_format  = NULL,
                      tz           = Sys.timezone(),
                      sep          = "auto",
                      skip         = "__auto__",
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
   colnms <- c("time", "accel_X", "accel_Y", "accel_Z")[!is.na(timeXYZ_cols)]

   # prepare col classes: when user does not supply time_format then POSIXct for time col, otherwise character
   colcls <- as.list(na.omit(timeXYZ_cols))
   if (is.null(time_format)) {
      names(colcls) <- c("POSIXct", rep("numeric", length(colcls) - 1))
   } else {
      names(colcls) <- c("character", rep("numeric", length(colcls) - 1))
   }

   # wrapper function to fread, setting parameters
   read_file <- function(f) {data.table::fread(file = f,
                                               select = timeXYZ_cols,
                                               tz = if (tz == "UTC") "UTC" else "", # fread only takes "UTC" or "" (system tz) --> extra step below needed
                                               col.names = colnms,
                                               colClasses = colcls,
                                               sep = sep,
                                               skip = skip,
                                               header = FALSE,
                                               nThread = n_fread_threads,
                                               ...
                                               )}

   # parallelization for reading the files:
   # given we have >1 files we will run fread in parallel via parallel::parLapply
   # in this case we set nThread = 1 in fread to avoid nested parallelization

   if (length(input) > 1) {
      n_fread_threads <- 1
      fread_cls <- parallel::makeCluster(data.table::getDTthreads())
      parallel::clusterExport(fread_cls, list("timeXYZ_cols", "tz", "colnms", "colcls", "sep", "skip", "n_fread_threads"), environment())
   } else {
      n_fread_threads <- data.table::getDTthreads()
   }

   private$dataDT <- data.table::rbindlist(parallel::parLapply(cl = fread_cls, input, read_file), idcol = "id")

   private$dataDT <- private$dataDT[complete.cases(private$dataDT), ]

   attr(private$dataDT$time, "tzone") <- tz

   # conversion to POSIX in case of user supplied time_format
   if (class(private$dataDT$time)[1] == "character") {
      private$dataDT[, time := as.POSIXct(time, format = time_format, tz = tz)]
   }

   # filter time range according to user-provided start and/or end time
   if (!is.null(start_time) & is.null(start_time)) {
      private$dataDT <- private$dataDT[time >= as.POSIXct(start_time, tz = tz), ]
   } else if (is.null(start_time) & !is.null(end_time)) {
      private$dataDT <- private$dataDT[time <= as.POSIXct(end_time, tz = tz)]
   } else if (!is.null(start_time) & !is.null(end_time)) {
      private$dataDT <- private$dataDT[(time >= as.POSIXct(start_time, tz = tz)) & (time <= as.POSIXct(end_time, tz = tz))]
   }

   # note availability of acceleration directions (for checks by other methods)
   private$has_data <- checkmate::checkDataTable(private$dataDT)
   private$has_X <- "accel_X" %in% colnames(private$dataDT)
   private$has_Y <- "accel_Y" %in% colnames(private$dataDT)
   private$has_Z <- "accel_Z" %in% colnames(private$dataDT)

   return(invisible(self))
}

