\name{bout_summary}
\alias{bout_summary}
\title{Output of \href{../../triact/html/Triact.html#method-summarize_bouts}{Triact$summarize_bouts()}}
\description{
  The information contained in the output table depends on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods. Some or all of the following columns might be present:

  \tabular{ll}{
  \strong{Column} \tab  \strong{Description} \cr
  \code{id} \tab Cow id \cr
  \code{bout_nr} \tab Sequential numbering of the bouts per cow id \cr
  \code{startTime} \tab Start time of the bout \cr
  \code{endTime} \tab End time of the bout \cr
  \code{duration} \tab Duration of the bout (units as specified via the \code{duration_units} argument) \cr
  \code{lying} \tab \code{TRUE} for lying bouts, \code{FALSE} for standing bouts \cr
  \code{side} \tab \code{"L"} for left lying side, \code{"R"} for right lying side (NA for standing bouts)\cr
  \code{meanL1DBA} \tab Mean of the L1 norms of the dynamic body acceleration vectors \cr
  \code{meanL2DBA} \tab Mean of the L2 norms of the dynamic body acceleration vectors\cr
  \code{meanL1Jerk} \tab Mean of the L1 norms of the jerk vectors\cr
  \code{meanL2Jerk} \tab Mean of the L2 norms of the jerk vectors\cr
 }

}



