# Dealing with “undefined global functions or variables”
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html

N <- acc_fwd <- acc_right <- acc_up <- boutDuration <- bout_nr <- delta_time <-
duration <- endTime <- gravity_up <- id <- lying <- private <- propI <- side <-
startTime <- add_lying <- self <- NULL

# https://stackoverflow.com/questions/43662416/when-using-data-table-in-a-package-r-cmd-check-notes-no-visible-global-functio
"." <- list

