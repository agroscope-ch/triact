\name{interval_summary}
\alias{interval_summary}
\title{Output of Triact$summarize_intervals()}
\description{
Output of \href{../../triact/html/Triact.html#method-summarize_intervals}{Triact$summarize_intervals()}. The information contained in the output table depends on the arguments \code{bouts} and \code{side} and on the analyses you added to the Triact object using the $add_activity(), $add_lying(), and $add_side() methods.

The asterisk (*) in the column names below stands for one of 'L1', 'L2', 'AdjL1', and 'AdjL2', indicating type of norm (L1, L2) and 'adjustment' of activity values to zero during lying, i.e. lying considered inactive by definition. DBA is the abbreviation for the dynamic body acceleration. See Simmler & Brouwers (2024).

  \bold{startTime}: Start time of the interval

  \bold{centerTime}: Center time of the interval (convenient for plotting)

  \bold{endTime}: End time of the interval

  \bold{duration}: Duration of data recordings in the interval. Helpful for the incompletely observed intervals at start and end of the recording. Units: As specified via the \code{duration_units} argument.

  \bold{durationStanding}: Duration in upright posture. Units: As specified via the \code{duration_units} argument.

  \bold{durationLying}: Duration in lying posture. Units: As specified via the \code{duration_units} argument.

  \bold{durationLyingLeft}: Duration in lying posture with lying side left. Units: As specified via the \code{duration_units} argument.

  \bold{durationLyingRight}: Duration in lying posture with lying side right. Units: As specified via the \code{duration_units} argument.

  \bold{mean*DBA}: Mean of the DBA-based proxy for physical activity. Units: \eqn{g}

  \bold{mean*Jerk}:Mean of the Jerk-based proxy for physical activity. Units: \eqn{g  s^{-1}}

  \bold{mean*DBAStanding}: Mean of the DBA-based proxy for physical activity when in upright posture. Units: \eqn{g}

  \bold{mean*JerkStanding}: Mean of the Jerk-based proxy for physical activity when in upright posture. Units: \eqn{g  s^{-1}}

  \bold{mean*DBALying}: Mean of the DBA-based proxy for physical activity when in lying posture. Units: \eqn{g}

  \bold{mean*JerkLying}: Mean of the Jerk-based proxy for physical activity when in lying posture. Units: \eqn{g  s^{-1}}

  \bold{mean*DBALyingLeft}: Mean of the DBA-based proxy for physical activity when in lying posture with lying side left. Units: \eqn{g}

  \bold{mean*JerkLyingLeft}: Mean of the Jerk-based proxy for physical activity when in lying posture with lying side left. Units: \eqn{g  s^{-1}}

  \bold{mean*DBALyingRight}: Mean of the DBA-based proxy for physical activity when in lying posture with lying side right. Units: \eqn{g}

  \bold{mean*JerkLyingRight}: Mean of the Jerk-based proxy for physical activity when in lying posture with lying side right. Units: \eqn{g  s^{-1}}

  \bold{nBoutsStanding}: Number of standing bouts (proportional if across intervals).

  \bold{nBoutsLying}: Number of lying bouts (proportional if across intervals).

  \bold{nBoutsLyingLeft}: Number of lying bouts with lying side left (proportional if across intervals).

  \bold{nBoutsLyingRight}: Number of lying bouts with lying side right (proportional if across intervals).

  \bold{wMeanDurationStandingBout}: Weighted-mean duration of standing bouts (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument.

  \bold{wMeanDurationLyingBout}: Weighted-mean duration of lying bouts (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument.

  \bold{wMeanDurationLyingBoutLeft}: Weighted-mean duration of lying bouts with lying side left (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument.

  \bold{wMeanDurationLyingBoutRight}: Weighted-mean duration of lying bouts with lying side right (weights are the proportions of the individual bouts overlapping with the respective intervals). Units: As specified via the \code{duration_units} argument.

}

    \references{
Simmler. M., Brouwers S. P., 2024. \emph{triact} package for R: Analyzing the lying behavior of cows from accelerometer data. PeerJ, 12:e17036. \doi{10.7717/peerj.17036}

}
