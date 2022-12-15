################################################################################
# summarize_... methods of Triact class
################################################################################

summarize_bouts <- function(bout_type = "both",
                            duration_units = "mins",
                            calc_for_incomplete = FALSE) {

  # check prerequisites --------------------------------------------------------

  if (!private$has("data")) {
    stop("No accelerometer data found. ",
         "Import data using methods $load_files() or $load_table().",
         call. = FALSE)
  }

  if (!private$has("lying")) {
    stop("No lying behaviour data found. ",
         "You need to call $add_lying() first.",
         call. = FALSE)
  }

  # check arguments ------------------------------------------------------------

  assertColl <- checkmate::makeAssertCollection()

  ## check bout_type
  checkmate::assertChoice(bout_type,
                          choices = c("both", "lying", "standing"),
                          add = assertColl)

  ## check duration_units
  checkmate::assertChoice(duration_units,
                          choices = c("secs", "mins", "hours"),
                          add = assertColl)

  ## check calc_for_incomplete
  checkmate::assertFlag(calc_for_incomplete,
                          add = assertColl)

  checkmate::reportAssertions(assertColl)

  # summarize data  ------------------------------------------------------------

  bout_select <- if (bout_type == "both") {
    TRUE
  } else if (bout_type == "lying") {
    private$dataDT$lying
  } else if (bout_type == "standing") {
    !private$dataDT$lying
  }

  col_calcs <- quote(list(startTime = minT,
                          endTime = maxT + private$sampInt,
                          duration  = {
                            interval_duration <- difftime(maxT, minT) + private$sampInt
                            units(interval_duration) <- duration_units
                            as.numeric(interval_duration)},
                          lying = unique(lying)))

  if (private$has("side")) col_calcs[["side"]] <- quote(unique(side))

  act_names <- c("L1DBA", "L2DBA", "L1Jerk", "L2Jerk")

  act_names <- c(act_names, paste0("Adj", act_names))

  for (act in act_names) {
    if (private$has(act)) {
      col_calcs[[paste0("mean", act)]] <- substitute(mean(act, na.rm = TRUE),
                                                     env = list(act = as.name(act)))
    }
  }

  analysis <- private$dataDT[bout_select, {
    {minT <- min(time); maxT <- max(time)} # block prepares temp vars
    eval(col_calcs)},                      # this is returned
    by = .(id, bout_nr)]

  # set summaries that depend on incompletely observed bouts to NA -------------

  if (!calc_for_incomplete) {

    analysis[, c("duration", "startTime", "endTime") := .(
      ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)), NA, duration),
      ifelse((bout_nr == 1), NA, startTime),
      ifelse((bout_nr == max(bout_nr)), NA, endTime)),
      by = id]

    for (act in act_names) {
      if (private$has(act)) {
        analysis[, paste0("mean", act) :=
                   ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)),
                          NA, get(paste0("mean", act))), by = id]
      }
    }

  }

  # return  --------------------------------------------------------------------

  return(transform_table(analysis))
}


################################################################################

summarize_intervals <- function(interval = "hour",
                                lag_in_s = 0,
                                duration_units = "mins",
                                bouts = FALSE,
                                side = FALSE,
                                calc_for_incomplete = FALSE) {

  # check prerequisites --------------------------------------------------------

  if (!private$has("data")) {
    stop("No accelerometer data found. ",
         "Import data using methods $load_files() or $load_table().",
         call. = FALSE)
  }

  if (isTRUE(bouts) & !private$has("lying")) {
    stop("Summary of bouts requested (bouts = TRUE) but lying data ",
         "is missing. You need to call $add_lying() first.",
         call. = FALSE)
  }

  if (isTRUE(side) & !private$has("side")) {
    stop("Summary by lying side requested (side = TRUE) but data ",
         "on lying side is missing. You need to call $add_side() first.",
         call. = FALSE)
  }

  # check arguments ------------------------------------------------------------

  ## check interval
  if (is(try(lubridate::floor_date(Sys.time(), interval), silent = TRUE),
         "try-error")) {
    stop(interval, " is not a valid interval. ",
         "Valid intervals are valid values for the 'unit' argument",
         "of lubridate::floor_date(). ",
         "Examples are '2 min', '1 day', '4 hours'. See ?lubridate::floor_date",
         call. = FALSE)
  }

  assertColl <- checkmate::makeAssertCollection()

  ## check lag_in_s
  checkmate::assertNumber(lag_in_s,
                         finite = TRUE,
                         add = assertColl)

  ## check duration_units
  checkmate::assertChoice(duration_units,
                          choices = c("secs", "mins", "hours"),
                          add = assertColl)
  ## check bouts
  checkmate::assertFlag(bouts,
                        add = assertColl)
  ## check side
  checkmate::assertFlag(side,
                        add = assertColl)

  ## check calc_for_incomplete
  checkmate::assertFlag(calc_for_incomplete,
                        add = assertColl)

  checkmate::reportAssertions(assertColl)

  # summarize data -------------------------------------------------------------

  col_calcs <- quote(list(centerTime = startTime +
                            (lubridate::duration(interval) / 2),
                          endTime = startTime + lubridate::duration(interval),
                          duration = data_duration))

  if (private$has("lying")) {
    col_calcs[["durationStanding"]] <- quote(mean(!lying) * data_duration)
    col_calcs[["durationLying"]] <- quote(mean(lying) * data_duration)
    if (private$has("side")) {
      col_calcs[["durationLyingLeft"]]  <-
        quote(mean(lying * (!is.na(side) & (side == "L"))) * data_duration)
      col_calcs[["durationLyingRight"]] <-
        quote(mean(lying * (!is.na(side) & (side == "R"))) * data_duration)
    }
  }

  act_names <- c("L1DBA", "L2DBA", "L1Jerk", "L2Jerk")

  act_names <- c(act_names, paste0("Adj", act_names))

  for (act in act_names) {
    if (private$has(act)) {
      col_calcs[[paste0("mean", act)]] <- substitute(mean(act, na.rm = TRUE),
                               env = list(act = as.name(act)))
    }
  }

  if (private$has("lying")) {

    for (act in act_names) {
      if (private$has(act)) {
        col_calcs[[paste0("mean", act, "Lying")]] <-
          substitute(mean(act[lying], na.rm = TRUE),
                     env = list(act = as.name(act)))
      }
    }

    for (act in act_names) {
      if (private$has(act)) {
        col_calcs[[paste0("mean", act, "Standing")]] <-
          substitute(mean(act[!lying], na.rm = TRUE),
                     env = list(act = as.name(act)))
      }
    }

    if (side) {

      for (act in act_names) {
        if (private$has(act)) {
        col_calcs[[paste0("mean", act, "LyingLeft")]] <-
          substitute(mean(act[(!is.na(side) & (side == "L"))], na.rm = TRUE),
                     env = list(act = as.name(act)))
        }
      }

      for (act in act_names) {
        if (private$has(act)) {
          col_calcs[[paste0("mean", act, "LyingRight")]] <-
            substitute(mean(act[(!is.na(side) & (side == "R"))], na.rm = TRUE),
                       env = list(act = as.name(act)))
        }
      }
    }
  }

  analysis <- private$dataDT[ ,
    {{minT = min(time); maxT = max(time)
      data_duration <- difftime(maxT, minT) + private$sampInt
      units(data_duration) <- duration_units
      data_duration <- as.numeric(data_duration)} # block prepares temp vars
     eval(col_calcs)},                            # this is returned
    by = .(id, startTime =
             lubridate::floor_date(time - lag_in_s, interval) + lag_in_s)]

  # summarize additional data on bouts -----------------------------------------

  if (bouts) {

    bout_x_interval <-
      private$dataDT[, .(lying = unique(lying),
                     side = if (private$has("side")) unique(side)
                            else character(), N = .N),
                     by = .(id, startTime =
                              lubridate::floor_date(time - lag_in_s, interval) +
                              lag_in_s, bout_nr)]

    bout_x_interval[, propI := N / sum(N), by = .(id, bout_nr)] # prop in interval

    bout_x_interval[self$summarize_bouts(bout_type = "both",
                                         duration_units = duration_units,
                                         calc_for_incomplete = calc_for_incomplete),
                    boutDuration := duration,
                    on = .(id, bout_nr)]

    col_calcs_b <-
      quote(list(nBoutsStanding =
                   if (any(is.na(boutDuration[!lying]))) as.double(NA)
                   else sum(propI[!lying]),
                 nBoutsLying =
                   if (any(is.na(boutDuration[lying]))) as.double(NA)
                   else sum(propI[lying]),
                 nBoutsLyingLeft =
                   if (any(is.na(boutDuration[lying]))) as.double(NA)
                   else sum(propI[lying & (!is.na(side) & (side == "L"))]),
                 nBoutsLyingRight =
                   if (any(is.na(boutDuration[lying]))) as.double(NA)
                   else sum(propI[lying & (!is.na(side) & (side == "R"))]),
                 wMeanDurationStandingBout =
                   sum((boutDuration[!lying] * propI[!lying])) / sum(propI[!lying]),
                 wMeanDurationLyingBout =
                   sum((boutDuration[lying] * propI[lying])) / sum(propI[lying]),
                 wMeanDurationLyingBoutLeft =
                   sum((boutDuration[lying & (!is.na(side) & (side == "L"))] *
                          propI[lying & (!is.na(side) & (side == "L"))])) /
                   sum(propI[lying & (!is.na(side) & (side == "L"))]),
                 wMeanDurationLyingBoutRight =
                   sum((boutDuration[lying & (!is.na(side) & (side == "R"))] *
                          propI[lying & (!is.na(side) & (side == "R"))])) /
                   sum(propI[lying & (!is.na(side) & (side == "R"))]))
                       )

    if (!side) {
      col_calcs_b[grepl("Right|Left", names(col_calcs_b))] <- NULL
    }

    analysis[bout_x_interval[, eval(col_calcs_b), by = .(id, startTime)],
             names(col_calcs_b)[-1] := mget(names(col_calcs_b)[-1]),
             on = .(id, startTime)]

  }


  # set summaries that depend on incompletely observed bouts to NA -------------

  if (!calc_for_incomplete) {

    not_affected <- c("id", "startTime", "centerTime", "endTime", "Duration")

    cols <- !colnames(analysis) %in% not_affected

    analysis[startTime %in% c(min(startTime), max(startTime)),
             colnames(analysis)[cols] := NA, by = id]

  }

  # return  --------------------------------------------------------------------

  return(transform_table(analysis))
}

# ----------------------------------------------------------------

