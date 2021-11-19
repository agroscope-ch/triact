\name{Triact}
\alias{Triact}
\title{R6 class for accelerometer data from cows}
\description{
R6 class for accelerometer data from cows

}
\details{
An object for containing and analyzing triaxial accelerometer data from hind legs of cows.
Analyses focus on standing and lying behaviour and activity of the cows.
}

\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{data}}{Raw and classified accelerometer data. Modified by $load_data, $add_lying, $add_side methods.}
}
\if{html}{\out{</div>}}
}

%% ------------------------------
%% ------------------------------

\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Triact$new()}}
\item \href{#method-load_data}{\code{Triact$load_data()}}
\item \href{#method-add_lying}{\code{Triact$add_lying()}}
\item \href{#method-add_side}{\code{Triact$add_side()}}
\item \href{#method-extract_liedown}{\code{Triact$extract_liedown()}}
\item \href{#method-extract_standup}{\code{Triact$extract_standup()}}
\item \href{#method-get_activity_by_iterval}{\code{Triact$get_activity_by_iterval()}}
\item \href{#method-get_activity_by_bout}{\code{Triact$get_activity_by_bout()}}
}
}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}

\subsection{Method \code{new()}}{
Create a new triact object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$new()}\if{html}{\out{</div>}}
}
\if{html}{\out{</div>}}
\subsection{Returns}{
A new `Triact` object.
}
}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-load_data"></a>}}
\if{latex}{\out{\hypertarget{method-load_data}{}}}

\subsection{Method \code{load_data()}}{
Import data from accelerometer files from one or several cows to the 'Triact' object. Multiple files from same cows is possible but data should follow each other without any gaps in time (overlap is allowed as duplicates after concatenation will be removed). The filenames must allow unique identification of the cow (parameter: id_substring).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$load_data(
  input,
  id_substring,
  start_time = NULL,
  end_time = NULL,
  timeXYZ_cols = c(1, 2, 3, 4),
  time_format = NULL,
  tz = Sys.timezone(),
  sep = "auto",
  skip = "__auto__",
  parallel = 1,
  ...
)}
\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{input}}{
Name of a file or a directory containing the files, which the data are to be read from. If it does not contain an absolute path, the directory or file name is relative to the current working directory, \code{getwd()}.
}

\item{\code{id_substring}}{
Character string containing a \link[=regex]{regular expression} (or character string) identifying the part in the file names representing the unique identifier of the cow. Alternatively, an integer vector identifying the substring by character position: c(first, last).
}

\item{\code{start_time}}{
Timestamp identifying the earliest data to keep, formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or other formats automatically tried by \link{as.POSIX*}. \emph{Default:} \code{NULL}
}


\item{\code{end_time}}{
Timestamp identifying the latest data to keep, formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or other formats automatically tried by \link{as.POSIX*}. \emph{Default:} \code{NULL}
}

\item{\code{timeXYZ_cols}}{
Integer vector specifying the columns containing the acceleratin time, and the X, Y, and Z axis acceleration data: c(time, X, Y, Z). \emph{Default:} \code{c(1, 2, 3, 4)}. Missing acceleration axes can be specified as \code{NA}
}

\item{\code{time_format}}{
Character vector specifying the date-time format corresponding to the acceleration files (syntax as in \code{\link[base]{strptime}}). If \code{NULL} a date-time format as tried by \link{as.POSIX*} is expected. \emph{Default:} \code{NULL}
}

\item{\code{tz}}{
Character vector specifying the \link[=timezones]{time zone} \emph{Default:} \code{Sys.timezone()}
}

\item{\code{sep}}{
The separator between columns in acceleration files. If "auto", it is automatically detected by \code{\link[=fread]{data.table::fread}}. \emph{Default:} \code{"auto"}
}

\item{\code{skip}}{
An integer indicating the number of lines to skip before reading data from the files. Alternatively, a (sub)string indicating the line to start reading data, or "__auto__" for the automatic detection by \code{\link[=fread]{data.table::fread}}.\emph{Default:} \code{"__auto__"}
}

\item{\code{parallel}}{
An integer indicating the number of files that are read in parallel. For \code{parallel > 1} the reading of the individual file is set to single-threaded to avoid nested parallelization. This behavior can be overwritten by passing \code{nThread} via \code{...}  argument, which is passed on to \code{\link[=fread]{data.table::fread}}. \emph{Default:} \code{1}
}


}}
}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_lying"></a>}}
\if{latex}{\out{\hypertarget{method-add_lying}{}}}
\subsection{Method \code{add_lying()}}{
Classify data into lying and upright and add results as column ‘lying’ to the ‘Triact’ object. Additionally, bouts (lying and upright) are uniquely numbered in column 'bout_id'. Classification procedures is composed of two steps: evaluating ‘Y-acceleration < \code{crit_lie}’ followed by running median smoothing with window size \code{k}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$add_lying(crit_lie = 0.5, k = 121, check = TRUE)}\if{html}{\out{</div>}}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{crit_lie}}{
Y-acceleration threshold for classifying lying versus upright. \emph{Default:} \code{0.5}
}

\item{\code{k}}{
Integer width of median window for smoothing; must be odd. \emph{Default:} \code{121}
}

\item{\code{check}}{
A logical value that indicates whether the data should be checked for possible incorrect mounting of the logger (180° turned) and corrected accordingly (negation of X and Y axis). \emph{Default:} \code{TRUE}}
}
}}}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_side"></a>}}
\if{latex}{\out{\hypertarget{method-add_side}{}}}
\subsection{Method \code{add_side()}}{
Classify lying bouts into left and right lying side and add results as column ‘side’ to the ‘Triact’ object. A lying bout is classified as 'L' (left) if 'Z-acceleration < crit_left' is predominantly true, otherwise it is classified as 'R' (right).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$add_side(crit_left = -0.5)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{crit_left}}{Z-acceleration threshold for classifying lying on left versus right side. \emph{Default:} \code{-0.5}}
}}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-extract_liedown"></a>}}
\if{latex}{\out{\hypertarget{method-extract_liedown}{}}}
\subsection{Method \code{extract_liedown()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$extract_liedown(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{sec_before}}{
\emph{Default:} \code{0}
}

\item{\code{sec_after}}{
\emph{Default:} \code{0}
}

}
}
\if{html}{\out{</div>}}
\subsection{Returns}{
A new `Person` object.
}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-extract_standup"></a>}}
\if{latex}{\out{\hypertarget{method-extract_standup}{}}}
\subsection{Method \code{extract_standup()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$extract_standup(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{sec_before}}{
\emph{Default:} \code{0}
}

\item{\code{sec_after}}{
\emph{Default:} \code{0}
}

}}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_activity_by_iterval"></a>}}
\if{latex}{\out{\hypertarget{method-get_activity_by_iterval}{}}}
\subsection{Method \code{get_activity_by_iterval()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$get_activity_by_iterval(interval = "hour", lag_in_s = 0)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{interval}}{default: \code{"hour"}}
\item{\code{lag_in_s}}{default: \code{0}}
}}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_activity_by_bout"></a>}}
\if{latex}{\out{\hypertarget{method-get_activity_by_bout}{}}}
\subsection{Method \code{get_activity_by_bout()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$get_activity_by_bout(bout_type = "all")}\if{html}{\out{</div>}}
}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{interval}}{default: \code{"hour"}}
\item{\code{lag_in_s}}{default: \code{0}}
}}
}
}
