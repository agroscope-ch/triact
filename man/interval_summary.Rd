\name{interval_summary}
\alias{interval_summary}
\title{Output of \href{../../triact/html/Triact.html#method-summarize_intervals}{Triact$summarize_intervals()}}
\description{
  The information contained in the output table depends on the arguments \code{bouts} and \code{side} and on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods. Some or all of the following columns might be present:

  \tabular{ll}{
  \strong{Column} \tab  \strong{Description} \cr
  \code{id} \tab Cow id \cr
  \code{startTime} \tab Start time of the interval \cr
  \code{centerTime} \tab Center time of the interval (convenient for plotting) \cr
  \code{endTime} \tab End time of the interval \cr
  \code{duration} \tab Duration of data recordings in the interval. Helpful for identifying incompletely observed intervals at start and end of the recording. Units: As specified via the \code{duration_units} argument.  \cr
  \code{durationStanding} \tab Duration in standing posture. Units: As specified via the \code{duration_units} argument.\cr
  \code{durationLying} \tab Duration in lying posture. Units: As specified via the \code{duration_units} argument. \cr
  \code{durationLyingLeft} \tab Duration in lying posture with lying side left. Units: As specified via the \code{duration_units} argument.  \cr
  \code{durationLyingRight} \tab Duration in lying posture with lying side right. Units: As specified via the \code{duration_units} argument.\cr
  \code{meanL1DBA} \tab Mean of the L1 norms of the dynamic body acceleration vectors. Units: \eqn{g} \cr
  \code{meanL2DBA} \tab Mean of the L2 norms of the dynamic body acceleration vectors. Units: \eqn{g} \cr
  \code{meanL1Jerk} \tab Mean of the L1 norms of the dynamic body acceleration vectors. Units: \eqn{g  s^{-1}} \cr
  \code{meanL2Jerk} \tab Mean of the L2 norms of the dynamic body acceleration vectors. Units: \eqn{g  s^{-1}} \cr
  \code{meanL1DBAStanding} \tab Mean of the L1 norms of the dynamic body acceleration vectors when in standing posture. Units: \eqn{g} \cr
  \code{meanL2DBAStanding} \tab Mean of the L2 norms of the dynamic body acceleration vectors when in standing posture. Units: \eqn{g} \cr
  \code{meanL1JerkStanding} \tab Mean of the L1 norms of the jerk vectors when in standing posture. Units: \eqn{g  s^{-1}} \cr
  \code{meanL2JerkStanding} \tab Mean of the L2 norms of the jerk vectors when in standing posture. Units: \eqn{g  s^{-1}} \cr
  \code{meanL1DBALying} \tab Mean of the L1 norms of the dynamic body acceleration vectors when in lying posture. Units: \eqn{g} \cr
  \code{meanL2DBALying} \tab Mean of the L2 norms of the dynamic body acceleration vectors when in lying posture. Units: \eqn{g} \cr
  \code{meanL1JerkLying} \tab Mean of the L1 norms of the jerk vectors when in lying posture. Units: \eqn{g  s^{-1}} \cr
  \code{meanL2JerkLying} \tab Mean of the L2 norms of the jerk vectors when in lying posture. Units: \eqn{g  s^{-1}} \cr
  \code{meanL1DBALyingLeft} \tab  Mean of the L1 norms of the dynamic body acceleration vectors when in lying posture with lying side left. Units: \eqn{g} \cr
  \code{meanL2DBALyingLeft} \tab  Mean of the L2 norms of the dynamic body acceleration vectors when in lying posture with lying side left. Units: \eqn{g} \cr
  \code{meanL1JerkLyingLeft} \tab Mean of the L1 norms of the jerk vectors when in lying posture with lying side left. Units: \eqn{g  s^{-1}} \cr
  \code{meanL2JerkLyingLeft} \tab Mean of the L2 norms of the jerk vectors when in lying posture with lying side left. Units: \eqn{g  s^{-1}} \cr
  \code{meanL1DBALyingRight} \tab Mean of the L1 norms of the dynamic body acceleration vectors when in lying posture with lying side right. Units: \eqn{g} \cr
  \code{meanL2DBALyingRight} \tab Mean of the L2 norms of the dynamic body acceleration vectors when in lying posture with lying side right. Units: \eqn{g} \cr
  \code{meanL1JerkLyingRight} \tab Mean of the L1 norms of the jerk vectors when in lying posture with lying side right. Units: \eqn{g  s^{-1}}  \cr
  \code{meanL2JerkLyingRight} \tab Mean of the L2 norms of the jerk vectors when in lying posture with lying side right. Units: \eqn{g  s^{-1}}  \cr
  \code{nBoutsStanding} \tab Number of standing bouts (proportional if across intervals). \cr
  \code{nBoutsLying} \tab Number of lying bouts (proportional if across intervals). \cr
  \code{nBoutsLyingLeft} \tab Number of lying bouts with lying side left (proportional if across intervals). \cr
  \code{nBoutsLyingRight} \tab Number of lying bouts with lying side right (proportional if across intervals). \cr
  \code{wMeanDurationStandingBout} \tab Weighted-mean duration of standing bouts (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument. \cr
  \code{wMeanDurationLyingBout} \tab Weighted-mean duration of lying bouts (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument. \cr
  \code{wMeanDurationLyingBoutLeft} \tab Weighted-mean duration of lying bouts with lying side left (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument. \cr
  \code{wMeanDurationLyingBoutRight} \tab Weighted-mean duration of lying bouts with lying side right (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument. \cr

 }

}


