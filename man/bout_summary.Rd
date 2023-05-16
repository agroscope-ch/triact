\name{bout_summary}
\alias{bout_summary}
\title{Output of Triact$summarize_bouts()}
\description{
Output of \href{../../triact/html/Triact.html#method-summarize_bouts}{Triact$summarize_bouts()}. The information contained in the output table depends on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods.

The asterisk (*) in the column names below stands for one of 'L1', 'L2', 'AdjL1', and 'AdjL2', indicating type of norm (L1, L2) and 'adjustment' of activity values to zero during lying, i.e. lying considered inactive by definition. DBA is the abbreviation for the dynamic body acceleration. See Simmler & Brouwers (2023).

DBA is the abbreviation for the dynamic body acceleration.

  \strong{id} Cow id

  \strong{bout_nr} Sequential numbering of the bouts per cow id

  \strong{startTime} Start time of the bout

  \strong{endTime} End time of the bout

  \strong{duration} Duration of the bout. Units: As specified via the \code{duration_units} argument.

  \strong{lying} \code{TRUE} for lying bouts, \code{FALSE} for standing bouts

  \strong{side} \code{"L"} for left lying side, \code{"R"} for right lying side (NA for standing bouts)

  \strong{mean*DBA} Mean of the DBA-based proxy for physical activity. Units: \eqn{g}

  \strong{mean*Jerk} Mean of the Jerk-based proxy for physical activity. Units: \eqn{g  s^{-1}}

}

    \references{
Simmler. M., Brouwers S., 2023. Triact package for R: Analyzing the lying behavior of cows from accelerometer data. \href{https://gitlab.com/AgroSimi/triact_manuscript}{pdf}

}

