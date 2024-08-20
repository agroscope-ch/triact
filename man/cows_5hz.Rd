\name{cows_5hz}
\alias{cows_5hz}
\title{Cow acceleration data}
\description{
  Acceleration data collected with triaxial accelerometers (MSR145, MSR Electronics, Switzerland) attached to the left hind leg of two dairy cows (cow01, cow02). The accelerometer sampling frequency was 5 Hz. The \emph{forward}, \emph{up}, and \emph{right} acceleration correspond to body relative directions as used in \emph{triact} (see \code{vignette("introduction", package = "triact")} and Simmler & Brouwers, 2024).

  \tabular{llll}{
    \emph{colname} \tab \emph{type} \tab \emph{description} \cr
    id \tab Factor \tab unique id for the cow \cr
    time \tab POSIXct \tab timestamp \cr
    acc_fwd \tab numeric \tab acceleration from \emph{forward} axis (units: \emph{g}) \cr
    acc_up \tab numeric \tab acceleration from \emph{up} axis (units: \emph{g}) \cr
    acc_right \tab numeric \tab acceleration from \emph{right} axis (units: \emph{g}) \cr
  }
}

\usage{
cows_5hz
}

\note{

From the raw data files distributed with the triact package, \code{cows_5hz} can be reproduced as follows:

\preformatted{
# create a Triact object
my_triact <- Triact$new()

input_dir <- system.file("extdata", package = "triact")

# load data from files
# note the mapping of XYZ to forward, up, right (parameter timeFwdUpRight_cols)

my_triact$load_files(input = input_dir,
                     id_substring = c(1, 5),
                     timeFwdUpRight_cols = c(1, -2, 3, -4),
                     tz = "Europe/Zurich",
                     skip = "DATA")

cows_5hz_recreated <- my_triact$data

# test whether they are identical
identical(cows_5hz_recreated, cows_5hz)

}
}

\source{Agroscope, 8356 Ettenhausen, Switzerland}

    \references{
Simmler. M., Brouwers S. P., 2024. \emph{triact} package for R: Analyzing the lying behavior of cows from accelerometer data. PeerJ, 12:e17036. \href{https://doi.org/10.7717/peerj.17036}{https://doi.org/10.7717/peerj.17036}

}
