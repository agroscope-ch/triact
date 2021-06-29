
setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/simi/triact_Mirjam_Fibl/example_concat")

library(data.table)

in_dir <- "MSR_raw"

files <- list.files()

names(files) <- sub('\\..*$', '', files)

i = 1

all_dat <- data.table::fread(files[i])

# -----------------------------------------
pre_process <- function(data_dt, crit_lie, crit_left, k) { 
  
  accel_cols = intersect(colnames(data_dt), c("Accel_X", "Accel_Y", "Accel_Z"))
  
  data_dt[, paste0(accel_cols, "_abs_diff") := lapply(.SD, function(d) c(NA, abs(diff(d)))), .SDcols = accel_cols]
  
  data_dt[, lying := runmed(Accel_Y > crit_lie, k, endrule = "constant")] 
  
  data_dt[, bout_id := cumsum(c(0, diff(lying) != 0))]
  
  data_dt[, side := if (lying[1] == 0) as.character(NA) else if (median(Accel_Z < crit_left)) "L" else "R", by = bout_id] 
  
  return(invisible(0)) # use for side effect !! 
  
}


# -----------------------------------------

all_dat$Accel_Z <- all_dat$Accel_Y

pre_process(all_dat, -0.5, 0.5, 120)

calc_activity_per_interval(all_dat, interval = "hours")

# -----------------------------------------


calc_activity_per_interval <- function(data_dt, interval, m_freq = 1) { # where should m_freq, the measurement frequency be determined
  
  accel_diff_cols = intersect(colnames(data_dt), c("Accel_X_abs_diff", "Accel_Y__abs_diff", "Accel_Z__abs_diff"))
  
  interval_activity_all <- 
    data_dt[, {dur_list <- .("duration" = .N * m_freq / 3600) 
               c(dur_list, .("activity" = sum(.SD) / dur_list$duration))}, 
               by = .("time" = as.factor(trunc(Time, units = interval))),
               .SDcols = accel_diff_cols]
  
    
    
  interval_activity_lie <- if (TRUE) {
    data_dt[, {dur_list <- .("duration_up" = (sum(lying == 0) * m_freq) / 3600,                                         # Zeitbestimmung so sinnvoll? 
                             "duration_lie" = (sum(lying == 1) * m_freq) / 3600)
               c(dur_list, .("activity_up" = sum(.SD[lying == 0]) / dur_list$duration_up,
                             "activity_lie" = sum(.SD[lying == 1]) / dur_list$duration_lie))},
               by = .("time" = as.factor(trunc(Time, units = interval))),
               .SDcols = accel_diff_cols]
  }
  
  
  interval_activity_LR <- if(TRUE) {  
    data_dt[, {dur_list <- .("duration_lie_L" = (sum(lying == 1 & side == "L") * m_freq) / 3600,
                             "duration_lie_R" = (sum(lying == 1 & side == "R") * m_freq) / 3600)
               c(dur_list, .("activity_lie_L" = sum(.SD[lying == 1 & side == "L"]) / dur_list$duration_lie_L,
                             "activity_lie_R" = sum(.SD[lying == 1 & side == "R"]) / dur_list$duration_lie_R))},
               by = .("time" = as.factor(trunc(Time, units = interval))),
               .SDcols = accel_diff_cols]
  
  }
  return(cbind(interval_activity_all, interval_activity_lie[,-1], interval_activity_LR[,-1]))
  
}

calc_activity_per_interval(all_dat, interval = "hours", 1)

# -----------------------------------------

# Hier weiter !!!! 

calc_activity_per_bout <- function(data_dt) {

  accel_diff_cols = intersect(colnames(data_dt), c("Accel_X_diff", "Accel_Y_diff", "Accel_Z_diff"))
  
  bout_activity <- 
    data_dt[, .("lying"      = unique(lying),
                "start_time" = min(Time),
                "duration"   = difftime(max(Time), min(Time), units = "hours"),
                "activity_sum" = sum(sapply(.SD, function(d) sum(abs(diff(na.omit(d))))))),
              by = bout_id, 
              .SDcols = accel_diff_cols]
  
  bout_activity[, ':='(activity = activity_sum / as.numeric(duration),
                       activity_sum = NULL)]
  
  return(bout_activity[])

}

# ----------------------------------------- 

triactControl <- function(
  crit_lie = -0.5,
  crit_left = -0.5,
  k = 120) {
  
  control <- as.list(environment())
  
  class(control) <- c("triactControl", "list")

  return(control)  
  
}
  

triact <- function(file, 
                   interval = "hours", 
                   bouts = "simple", 
                   control = triactControl()) {
  
  list2env(control, environment())
  
  all_dat <- data.table::fread(file)
  
  add_delta(all_dat)
  
  if (!is.null(interval)) {
  
    interval_activity <- calc_activity_per_interval(all_dat, interval)
    
  }
  
  if ((!is.null(bouts)) & (bouts == "simple")) {
  
    add_bouts(all_dat, crit_lie, k)

    bout_activity <- calc_activity_per_bout(all_dat)

  }
  
  return(list(interval_activity, bout_activity))
  
}

triact(file = "MSR325217.csv")



