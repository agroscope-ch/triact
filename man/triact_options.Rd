\name{triact_options}
\alias{triact_options}
\title{Package options}
\description{
Global options that affect the triact R package.
}

\section{Options used in triact}{
  \describe{
    \item{\code{triact_table}}{Type of tables returned by triact can be specified. Options are "data.frame" (the default), "tibble", and "data.table"}

    \item{\code{triact_tolerance}}{A tolerance value used in determining the sampling interval in the $load_files() and $load_table() methods (default 0.05).

The sampling interval in the accelerometer raw data may be subject to small (random) shifts from the set value. The median of the observed sampling intervals is therefore used for internal calculations of e.g. smoothing parameters. The tolerance value is used to check for larger deviations from this median sampling frequency, as this may indicate problems such as gaps in the data or sensors set to different sampling frequencies. If the deviation of at least one observed interval is larger than the tolerance value times the median sampling interval an error is raised.}

}}


\section{Examples}{
  \preformatted{

  options(triact_table = "data.frame")

  options(triact_tolerance = 0.05)

  }
}
