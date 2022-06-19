\name{Triact}
\alias{Triact}
\title{R6 class for analyzing accelerometer data from cows}
\description{
An object for containing and analyzing data from accelerometers attached to the leg of cows.
Analyses focus on the lying behaviour and on the cow's 'activity'.
}

\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{data}}{Raw accelerometer data and derived information. Modified by $load_data and the $add_... methods}
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
\item \href{#method-add_activity}{\code{Triact$add_activity()}}
\item \href{#method-add_lying}{\code{Triact$add_lying()}}
\item \href{#method-add_side}{\code{Triact$add_side()}}
\item \href{#method-extract_liedown}{\code{Triact$extract_liedown()}}
\item \href{#method-extract_standup}{\code{Triact$extract_standup()}}
\item \href{#method-summarize_intervals}{\code{Triact$summarize_intervals()}}
\item \href{#method-summarize_bouts}{\code{Triact$summarize_bouts()}}
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
Import data from accelerometer data files from one or several cows to the 'Triact' object. Importing multiple files from the same cows is possible but data should follow each other without any gaps in time (overlap is allowed as duplicates after concatenation will be removed). The filenames must allow unique identification of the cow (parameter: id_substring). Sampling frequency must be consistent across the files. Triaxial, biaxial and uniaxial axelerometer data are allowed, but only triaxial data corresponding to relative body coordinates allow full functionality.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$load_data(
  input,
  id_substring,
  timeFwdUpRight_cols = c(1, 2, 3, 4),
  time_format  = NULL,
  tz           = Sys.timezone(),
  start_time   = NULL,
  end_time     = NULL,
  sep          = "auto",
  skip         = "__auto__",
  parallel     = 1,
  ...
)}
\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{input}}{
Specifies the input acceleration data files. Character vector with the name(s) of the file(s) or a directory containing the files (files can be in subdirectories). If it does not contain absolute paths, the directory or file name(s) are relative to the current working directory, \code{getwd()}.
}

\item{\code{id_substring}}{
Integer vector identifying the substring of the file names representing the unique identifier of the cows by character position: c(first, last), e.g, \code{c(1, 5)} for first to fifth character. Alternatively, a Perl-like \link[=regex]{regular expression} matching the substring.
}

\item{\code{timeFwdUpRight_cols}}{
Integer vector specifying the columns containing the time, and the forward, up, and right axis acceleration data: c(time, fwd, up, right). Missing acceleration axes are specified as \code{NA}. A negative mathematical sign is used to indicate that the recorded data reflects the opposite direction (e.g., for backward acceleration, specify the forward acceleration column with a negative mathematical sign). \emph{Default:} \code{c(1, 2, 3, 4)}
}

\item{\code{time_format}}{
Character vector specifying the date-time format corresponding to the acceleration files (syntax as in \code{\link[base]{strptime}}). If \code{NULL} a date-time format as tried by \link{as.POSIX*} is expected. \emph{Default:} \code{NULL}
}

\item{\code{tz}}{
Character vector specifying the \link[=timezones]{time zone} \emph{Default:} \code{Sys.timezone()}
}

\item{\code{start_time}}{
Time from which on the data should be considered. Formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or in another format automatically tried by \link{as.POSIX*}. \emph{Default:} \code{NULL}
}


\item{\code{end_time}}{
Time up to which the data should be considered. Formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or in another format automatically tried by \link{as.POSIX*}. \emph{Default:} \code{NULL}
}


\item{\code{sep}}{
The separator between columns in the acceleration data files. If "auto", it is automatically detected by \code{\link[=fread]{data.table::fread}}. \emph{Default:} \code{"auto"}
}

\item{\code{skip}}{
An integer indicating the number of lines to skip before reading data from the files. Alternatively, a (sub)string indicating the line to start reading data, or "__auto__" for the automatic detection by \code{\link[=fread]{data.table::fread}}.\emph{Default:} \code{"__auto__"}
}

\item{\code{parallel}}{
An integer indicating the number of files that are read in parallel. For \code{parallel > 1} the reading of the individual file is set to single-threaded to avoid nested parallelization. This behavior can be overwritten by additionally passing \code{nThread} via \code{...}  argument, which is passed on to \code{\link[=fread]{data.table::fread}}. \emph{Default:} \code{1}
}


}}
}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_activity"></a>}}
\if{latex}{\out{\hypertarget{method-add_activity}{}}}
\subsection{Method \code{add_activity()}}{
Calculate the cow's 'activity' and add results as column to the 'Triact' object. Activity is defined as the magnitude of the jerk vector, which is estimated from the acceleration vector by approximating the derivatives using the finite differences.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$add_activity(add_jerk = FALSE)}\if{html}{\out{</div>}}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{add_jerk}}{
A logical value that indicates whether the jerk vector (fwr_jerk, up_jerk, right_jerk) should be added to the 'Triact' object. \emph{Default:} \code{FALSE}
}

}
}}}


%% ------------------------------
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_lying"></a>}}
\if{latex}{\out{\hypertarget{method-add_lying}{}}}
\subsection{Method \code{add_lying()}}{
Classify data into lying and standing and add results as column ‘lying’ to the ‘Triact’ object. Additionally, bouts (lying and standing) are uniquely numbered in column 'bout_id'. Classification procedure is composed of two steps: evaluating ‘up-acceleration < \code{crit_lie}’ followed by running median smoothing with window size \code{k}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$add_lying(crit_lie = 0.5, k = 121, check = TRUE)}\if{html}{\out{</div>}}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{crit_lie}}{
Up-acceleration threshold for classifying into lying and standing. \emph{Default:} \code{0.5}
}

\item{\code{k}}{
Integer width of median window for smoothing; must be odd. \emph{Default:} \code{121}
}

\item{\code{check}}{
A logical value that indicates whether the data should be checked for possible incorrect mounting of the logger (180° turned in saggital plane) and corrected accordingly (negation of forward and upward axis). \emph{Default:} \code{TRUE}}
}
}}}

%% ------------------------------

\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add_side"></a>}}
\if{latex}{\out{\hypertarget{method-add_side}{}}}
\subsection{Method \code{add_side()}}{
Classify lying bouts into left and right lying side and add results as column ‘side’ to the ‘Triact’ object. A lying bout is classified as 'L' (left) if 'right-acceleration < crit_left' is predominantly true, otherwise it is classified as 'R' (right).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$add_side(left_leg, crit_left = if(left_leg) -0.5 else 0.5)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{left_leg}}{Logical indicating whether the loggers were attached to the left hind leg (\code{TRUE}) or to the right hind leg (\code{FALSE}). This information is used to choose the default value for \code{crit_left}. It is ignored if \code{crit_left} is specified by the user.}
\item{\code{crit_left}}{Right-acceleration threshold for classifying lying on left versus right side. \emph{Default:} \code{-0.5} if \code{left_leg} is \code{TRUE}, else \code{0.5}}
}}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-extract_liedown"></a>}}
\if{latex}{\out{\hypertarget{method-extract_liedown}{}}}
\subsection{Method \code{extract_liedown()}}{
Extracts data associated with liedown events (standing-to-lying transitions). Operates in two modes, see \emph{Returns} section.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$extract_liedown(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{sec_before}}{
From how many seconds before the liedown events data should be considered. \emph{Default:} \code{0}
}

\item{\code{sec_after}}{
Until how many seconds after the liedown events data should be considered. \emph{Default:} \code{0}
}

}
}
\if{html}{\out{</div>}}
\subsection{Returns}{
With default settings, a table with one entry per liedown event, with timestamp and bout_id of the lying bout, plus lying side information (if available). With parameters \code{sec_before} and/or \code{sec_after} > 0, a list containing individual tables per liedown event. These tables are extracts of all data in the 'Triact' object from within the defined time window around the liedown events.
}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-extract_standup"></a>}}
\if{latex}{\out{\hypertarget{method-extract_standup}{}}}
\subsection{Method \code{extract_standup()}}{
Extracts data associated with standup events (lying-to-standing transitions). Operates in two modes, see \emph{Returns} section.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$extract_standup(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{

\item{\code{sec_before}}{
From how many seconds before the standup events data should be considered. \emph{Default:} \code{0}
}

\item{\code{sec_after}}{
Until how many seconds after the standup events data should be considered. \emph{Default:} \code{0}
}

}
}
\if{html}{\out{</div>}}
\subsection{Returns}{
With default settings, a table with one entry per standup event, with timestamp and bout_id of the lying bout, plus lying side information (if available). With parameters \code{sec_before} and/or \code{sec_after} > 0, a list containing individual tables per standup event. These tables are extracts of all data in the 'Triact' object from within the defined time window around the standup events.
}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summarize_intervals"></a>}}
\if{latex}{\out{\hypertarget{method-summarize_intervals}{}}}
\subsection{Method \code{summarize_intervals()}}{
Summarizes the data in the 'Triact' object by regular intervals.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$summarize_intervals(interval = "hour", lag_in_s = 0, duration_units = "mins")}\if{html}{\out{</div>}}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{interval}}{Character string specifying the intervals to be analysed. Any unique English abbreviations as in \code{\link[=period]{lubridate::period}} are allowed, e.g., \code{"hour"}, \code{"min"}, \code{"10 mins"}, and \code{"0.5 hours"}. \emph{Default:} \code{"hour"}}
\item{\code{lag_in_s}}{Lag in seconds with respect to the full hour. \emph{Default:} \code{0}}
\item{\code{duration_units}}{Unit in which durations should be returned. Options are \code{"secs"}, \code{"mins"} and \code{"hours"}. \emph{Default:} \code{"mins"}}
}}
\if{html}{\out{</div>}}
\subsection{Returns}{
A table summarizing the data in the 'Triact' object by interval. \cr\cr

\strong{Note 1}: The \code{duration} column informs on the duration with data available. At the beginning and end of your data aquisition the interval might not be fully covered.\cr\cr

\strong{Note 2}: The information contained in the table depends on the data you added to the 'Triact' object using the $add_... methods


}
}

%% ------------------------------

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summarize_bouts"></a>}}
\if{latex}{\out{\hypertarget{method-summarize_bouts}{}}}
\subsection{Method \code{summarize_bouts()}}{
Summarizes the data in the 'Triact' object by lying/standing bouts.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Triact$summarize_bouts(bout_type = "both", duration_units = "mins", calc_for_incomplete = FALSE)}\if{html}{\out{</div>}}
}
\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bout_type}}{ Type of bout to be considered. Options are \code{"both"}, \code{"lying"}, and \code{"standing"}. \emph{Default:} \code{"both"}}
\item{\code{duration_units}}{Unit in which durations should be returned. Options are \code{"secs"}, \code{"mins"}, and \code{"hours"}. \emph{Default:} \code{"mins"}}
\item{\code{calc_for_incomplete}}{Logical indicating whether a complete summary should also be returned for the incompletely observed bouts (first and last bout for each cow). \emph{Default:} \code{FALSE}}
}}
}
}

%% ------------------------------



