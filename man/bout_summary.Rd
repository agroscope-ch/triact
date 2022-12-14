\name{bout_summary}
\alias{bout_summary}
\title{Output of \href{../../triact/html/Triact.html#method-summarize_bouts}{Triact$summarize_bouts()}}
\description{
  The information contained in the output table depends on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods.

The asterisk (*) in the column names below stands for one of 'L1', 'L2', 'AdjL1', and 'AdjL2'. DBA is the abbreviation for the dynamic body acceleration.

  \tabular{ll}{
  \strong{Column} \tab  \strong{Description} \cr
  \code{id} \tab Cow id \cr
  \code{bout_nr} \tab Sequential numbering of the bouts per cow id \cr
  \code{startTime} \tab Start time of the bout \cr
  \code{endTime} \tab End time of the bout \cr
  \code{duration} \tab Duration of the bout (units as specified via the \code{duration_units} argument) \cr
  \code{lying} \tab \code{TRUE} for lying bouts, \code{FALSE} for standing bouts \cr
  \code{side} \tab \code{"L"} for left lying side, \code{"R"} for right lying side (NA for standing bouts)\cr
  \code{mean*DBA} \tab Mean of the DBA-based proxy for physical activity \cr
  \code{mean*Jerk} \tab Mean of the Jerk-based proxy for physical activity\cr
 }

}



