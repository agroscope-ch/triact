\name{cows_5hz}
\alias{cows_5hz}
\title{cow acceleration data}
\description{
  Data collected with a triaxial accelerometers (MSR145, MSR Electronics, Switzerland) attached to the left hind leg of two dairy cows (cow01, cow02). The accelerometer sampling frequency was 5 Hz.

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

dir <- system.file("extdata", package = "triact")

my_triact$load_files(input = dir,
                     id_substring = c(1, 5),
                     timeFwdUpRight_cols = c(1, 2 ,3, 4),
                     skip = "*DATA")

cows_5hz_recreated <- my_triact$data

# test whether they are identical
identical(cows_5hz_recreated, cows_5hz)

}
}

\source{Agroscope, 8356 Ettenhausen, Switzerland}
