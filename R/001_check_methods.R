
check_orientation <- function(crit = 0.5) {

  # argument checks ------------------------------------------------------------

  checkmate::assertNumber(crit)

  # check wrong mounting of logger and correct (180° turned in sagittal plane)--
  check <- parse(text = "sum(acc_up > crit) < sum(acc_up < -1 * crit)")
  up_inverted <- private$dataDT[, .(test = eval(check)), id]
  if (any(up_inverted$test)) {

    message("This function checks for a specific type of incorrect mounting of the potentiometer, namely accidental mounting rotated by 180° in the sagittal plane.")

    warning(paste("For the IDs listed below the the logger seem to have been attached rotated:\n\n"
                  , paste(up_inverted$id[up_inverted$test], collapse = ", ")),
            call. = FALSE)
    private$dataDT[, c("acc_up", "acc_fwd") := if(eval(check)) .(-acc_up, -acc_fwd) else .(acc_up, acc_fwd), id]
    warning(paste("For the IDs listed below the upward- and forward-axis were automatically negated (multiplied by -1) because the data appeared to come from a logger that was mounted 180° rotated (see package documentation):\n\n"
                  , paste(up_inverted$id[up_inverted$test], collapse = ", ")),
            call. = FALSE)
  }
}

