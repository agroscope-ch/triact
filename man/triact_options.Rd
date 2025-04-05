\name{triact_options}
\alias{triact_options}
\title{Package options}
\description{
Global options that affect the triact R package.
}

\section{Options used in triact}{
  \describe{
    \item{\code{triact_table}}{Type of tables returned by triact. Options are "data.frame" (the default), "tibble", and "data.table"}

    \item{\code{triact_tolerance}}{A tolerance value used in determining the sampling interval in the $load_files() and $load_table() methods (default 0.5).

Details: The sampling interval (\ifelse{html}{\out{= frequency<sup>-1</sup>}}{\eqn{freq^-1}}) in the accelerometer raw data may be subject to small (random) shifts from the set value. The median of the observed sampling intervals is therefore used for internal calculations of e.g. smoothing parameters. The tolerance value is used to check for larger deviations from this median sampling interval, as this may indicate problems such as gaps in the data or accelerometers set to different sampling frequencies. An error is raised if the deviation of at least one observed interval is larger than the tolerance value times the median sampling interval.}

}}


\section{Examples}{
  \preformatted{

  options(triact_table = "data.frame")

  options(triact_tolerance = 0.5)

  }
}
