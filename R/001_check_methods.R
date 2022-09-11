# ------------------------------------------------------------------------------
# check_... methods of Triact class
# ------------------------------------------------------------------------------

################################################################################

check_orientation <- function(crit = 0.5) {

  # argument checks ------------------------------------------------------------

  checkmate::assertNumber(crit)

  # check for 180° rotation in sag ital plain ----------------------------------
  check <- parse(text = "sum(acc_up > crit) < sum(acc_up < -1 * crit)")

  up_inverted <- private$dataDT[, .(test = eval(check)), id]

  # correct and remove (previously added) analyses if dependent ----------------

  message("This method checks for potential incorrect mounted accelerometers, rotated 180° in the sagital plane.\n")

  if (any(up_inverted$test)) {

    message(paste("For the IDs listed below the accelerometers seem to have been attached rotated:\n"
                  , paste(up_inverted$id[up_inverted$test], collapse = ", "), "\n"))

    message("Should the upward and forward axis be negative (multiplied by -1) to correct?")

    ans <- NA
    while (!ans %in% c(0, 1)) {
      ans <- suppressWarnings(as.numeric(readline("0: no, 1: yes ")))
    }

    if (ans) {
      private$dataDT[, c("acc_up", "acc_fwd") :=
                       if(eval(check)) .(-acc_up, -acc_fwd) else .(acc_up, acc_fwd), id]

      message("Correction done!")

      if (private$has_lying) {
        suppressWarnings(
          private$dataDT[, c("bout_nr", "lying", "acc_up_filtered") := NULL])
        private$has_lying <- FALSE
        warning("Information on lying bouts removed. Please re-run $add_lying().",
                call. = FALSE)
      }

      if (private$has_side) {
        private$dataDT[, side := NULL]
        private$has_side <- FALSE
        warning("Information on lying side removed. Please re-run $add_side().",
                call. = FALSE)
      }

      if ("jerk_up" %in% colnames(private$dataDT)) {
        private$dataDT[, c("jerk_up", "jerk_fwd", "jerk_right") := NULL]
        warning("Information on jerk removed. Please re-run $add_activity(add_jerk = TRUE).",
                call. = FALSE)
      }
    }
  } else{
    message("No incorrectly mounted accelerometers found.")
  }
}

