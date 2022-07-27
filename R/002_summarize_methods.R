# ----------------------------------------------------------------
# summarize_... methods of Triact class
# ----------------------------------------------------------------

summarize_intervals <- function(interval = "hour",
                                lag_in_s = 0,
                                duration_units = "mins",
                                bouts = FALSE,
                                calc_for_incomplete = FALSE) {

  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertNumber(lag_in_s, finite = TRUE)
  checkmate::assertChoice(duration_units, c("secs", "mins", "hours"))
  checkmate::assertFlag(bouts)
  if (bouts & !private$has_lying) {
    stop("Summary of bouts requested (bouts = TRUE) but lying data is missing. You need to call $add_lying() first.", call. = FALSE)
    }

  col_calcs <- quote(list(centerTime = startTime + (lubridate::duration(interval) / 2),
                          endTime = startTime + lubridate::duration(interval),
                          duration = data_duration,
                          durationStanding = mean(!lying) * data_duration,
                          durationLying = mean(lying) * data_duration,
                          durationLyingLeft = mean(lying * (!is.na(side) & (side == "L"))) * data_duration,
                          durationLyingRight = mean(lying * (!is.na(side) & (side == "R"))) * data_duration,
                          meanActivity = mean(activity, na.rm = TRUE),
                          meanActivityStanding = mean(activity[!lying], na.rm = TRUE),
                          meanActivityLying = mean(activity[lying], na.rm = TRUE),
                          meanActivityLyingLeft = mean(activity[(!is.na(side) & (side == "L"))], na.rm = TRUE),
                          meanActivityLyingRight = mean(activity[(!is.na(side) & (side == "R"))], na.rm = TRUE))
                     )

  if (!private$has_lying) col_calcs[c("durationStanding", "durationLying")] <- NULL
  if (!private$has_side) col_calcs[c("durationLyingLeft", "durationLyingRight")] <- NULL
  if (!private$has_activity) col_calcs["meanActivity"] <- NULL
  if (!private$has_activity | !private$has_lying) col_calcs[c("meanActivityStanding", "meanActivityLying")] <- NULL
  if (!private$has_activity | !private$has_side) col_calcs[c("meanActivityLyingLeft", "meanActivityLyingRight")] <- NULL

  analysis <- private$dataDT[ , {{minT = min(time); maxT = max(time)
                                  data_duration <- difftime(maxT, minT) + private$sampInt
                                  units(data_duration) <- duration_units
                                  data_duration <- as.numeric(data_duration)} # block prepares temp vars
                                 eval(col_calcs)},                                    # this is returned
                              by = .(id, startTime = lubridate::floor_date(time - lag_in_s, interval) + lag_in_s)]

  # --------

  if (bouts) {

    # if (lubridate::duration(interval, units = "hours") < 6) {
    #   message("For meaningful analysis of bouts per interval (bouts == TRUE) you might want to consider a larger interval, i.e. interval = 'day'.")
    # }

    bout_x_interval <- private$dataDT[, .(lying = unique(lying), side = if (private$has_side) unique(side) else character(), N = .N),
                                      by = .(id, startTime = lubridate::floor_date(time - lag_in_s, interval) + lag_in_s, bout_nr)]

    bout_x_interval[, proportion_in_interval := N / sum(N), by = .(id, bout_nr)]

    bout_x_interval[self$summarize_bouts(bout_type = "both", duration_units = duration_units, calc_for_incomplete = calc_for_incomplete),
                    boutDuration := duration,
                    on = .(id, bout_nr)]

    col_calcs <- quote(list(nBoutsStanding = if (any(is.na(boutDuration[!lying]))) as.double(NA) else sum(proportion_in_interval[!lying]),
                            nBoutsLying = if (any(is.na(boutDuration[lying]))) as.double(NA) else sum(proportion_in_interval[lying]),
                            nBoutsLyingLeft = if (any(is.na(boutDuration[lying]))) as.double(NA) else sum(proportion_in_interval[lying & (!is.na(side) & (side == "L"))]),
                            nBoutsLyingRight = if (any(is.na(boutDuration[lying]))) as.double(NA) else sum(proportion_in_interval[lying & (!is.na(side) & (side == "R"))]),
                            wMeanDurationStandingBout = sum((boutDuration[!lying] * proportion_in_interval[!lying])) / sum(proportion_in_interval[!lying]),
                            wMeanDurationLyingBout = sum((boutDuration[lying] * proportion_in_interval[lying])) / sum(proportion_in_interval[lying]),
                            wMeanDurationLyingBoutLeft = sum((boutDuration[lying & (!is.na(side) & (side == "L"))] * proportion_in_interval[lying & (!is.na(side) & (side == "L"))])) / sum(proportion_in_interval[lying & (!is.na(side) & (side == "L"))]),
                            wMeanDurationLyingBoutRight = sum((boutDuration[lying & (!is.na(side) & (side == "R"))] * proportion_in_interval[lying & (!is.na(side) & (side == "R"))])) / sum(proportion_in_interval[lying & (!is.na(side) & (side == "R"))]))
                       )

    if (!private$has_side) col_calcs[c("nBoutsLyingLeft", "nBoutsLyingRight", "wMeanDurationLyingBoutLeft", "wMeanDurationLyingBoutRight")] <- NULL

    analysis[bout_x_interval[, eval(col_calcs), by = .(id, startTime)],
             names(col_calcs)[-1] := mget(names(col_calcs)[-1]),
             on = .(id, startTime)]

  }

  if (!calc_for_incomplete) {

    cols <- colnames(analysis) %in% c("durationStanding",
                                      "durationLying",
                                      "durationLyingLeft",
                                      "durationLyingRight",
                                      "meanActivity",
                                      "meanActivityStanding",
                                      "meanActivityLying",
                                      "meanActivityLyingLeft",
                                      "meanActivityLyingRight",

                                      "nBoutsStanding",
                                      "nBoutsLying",
                                      "nBoutsLyingLeft",
                                      "nBoutsLyingRight",
                                      "wMeanDurationStandingBout",
                                      "wMeanDurationLyingBout",
                                      "wMeanDurationLyingBoutLeft",
                                      "wMeanDurationLyingBoutRight")

    analysis[startTime %in% c(min(startTime), max(startTime)), colnames(analysis)[cols] := NA, by = id]

  }

  return(transform_table(analysis))
}

# ----------------------------------------------------------------

summarize_bouts <- function(bout_type = "both",
                            duration_units = "mins",
                            calc_for_incomplete = FALSE) {

  checkmate::assertTRUE(private$has_data, .var.name = "has data?")
  checkmate::assertTRUE(private$has_lying, .var.name = "lying added?")
  checkmate::assertChoice(bout_type, choices = c("both", "lying", "standing"))
  checkmate::assert_choice(duration_units, c("secs", "mins", "hours"))

  bout_select <- if (bout_type == "both") {
    TRUE
  } else if (bout_type == "lying") {
    private$dataDT$lying
  } else if (bout_type == "standing") {
    !private$dataDT$lying
  }

  col_calcs <- quote(list(startTime = minT,
                       endTime = maxT + private$sampInt,
                       duration  = {interval_duration <- difftime(maxT, minT) + private$sampInt
                                    units(interval_duration) <- duration_units
                                    as.numeric(interval_duration)},
                       meanActivity  = mean(activity, na.rm = TRUE),
                       lying = unique(lying),
                       side = unique(side)))

  if (!private$has_activity) col_calcs["meanActivity"] <- NULL
  if (!private$has_side) col_calcs["side"] <- NULL

  analysis <- private$dataDT[bout_select, {{minT <- min(time); maxT <- max(time)} # block prepares temp vars
                                           eval(col_calcs)},                      # this is returned
                             by = .(id, bout_nr)]

  if (!calc_for_incomplete) {

    analysis[, c("duration", "startTime", "endTime") := .(
                 ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)), NA, duration),
                 ifelse((bout_nr == 1), NA, startTime),
                 ifelse((bout_nr == max(bout_nr)), NA, endTime)),
              by = id]

    if (private$has_activity) {

      analysis[, meanActivity := ifelse((bout_nr == 1) | (bout_nr == max(bout_nr)), NA, meanActivity), by = id]

    }

  }

  return(transform_table(analysis))
}




