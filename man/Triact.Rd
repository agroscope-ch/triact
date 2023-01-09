\name{Triact}
\alias{Triact}
\title{R6 class for analyzing accelerometer data from cows}
\description{
  An object for containing and analyzing data from accelerometers attached to a hind leg of cows. Analyses focus on the lying behaviour and on the cow's level of physical activity as detailed in Simmler & Brouwers (2023).
}

\section{Active bindings}{
  \if{html}{\out{<div class="r6-active-bindings">}}
  \describe{
    \item{\code{data}}{Raw accelerometer data and analysis results. Mainly modified by $load_... and the $add_... methods}
  }
  \if{html}{\out{</div>}}
}


%% ------------------------------

\section{Methods}{
  \subsection{Public methods}{
    \itemize{
      \item \href{#method-new}{\code{Triact$new()}}
      \item \href{#method-load_files}{\code{Triact$load_files()}}
      \item \href{#method-load_table}{\code{Triact$load_table()}}
      \item \href{#method-check_orientation}{\code{Triact$check_orientation()}}
      \item \href{#method-add_lying}{\code{Triact$add_lying()}}
      \item \href{#method-add_side}{\code{Triact$add_side()}}
      \item \href{#method-add_activity}{\code{Triact$add_activity()}}
      \item \href{#method-summarize_intervals}{\code{Triact$summarize_intervals()}}
      \item \href{#method-summarize_bouts}{\code{Triact$summarize_bouts()}}
      \item \href{#method-extract_liedown}{\code{Triact$extract_liedown()}}
      \item \href{#method-extract_standup}{\code{Triact$extract_standup()}}
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
    \subsection{Returns}{
      A new `Triact` object.
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-load_files"></a>}}
  \if{latex}{\out{\hypertarget{method-load_files}{}}}

  \subsection{Method \code{load_files()}}{
    Import acceleration data files from one or several cows to the 'Triact' object. Importing multiple files from the same cows is possible but data should follow each other without any gaps in time (overlap is allowed as duplicates after concatenation will be removed). The filenames must allow unique identification of the cow (parameter: \code{id_substring}). Accelerometer sampling frequency must be consistent across the files. Acceleration should be in units of \emph{g}. Triaxial, biaxial and uniaxial accelerometer data are allowed, but only triaxial data corresponding to relative body directions allows full functionality.

\emph{Important}: Make sure to correctly specify how to map the axes as named by the accelerometer (e.g., x, y, z) to the body relative axes as used in triact (parameter: \code{timeFwdUpRight_cols}). See Simmler & Brouwers (2023) for an illustration.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$load_files(
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
          Character vector specifying the date-time format corresponding to the acceleration files (syntax as in \code{\link[base]{strptime}}). If \code{NULL} a date-time format as tried by \link[=as.POSIXct]{as.POSIX*} is expected. \emph{Default:} \code{NULL}
        }

        \item{\code{tz}}{
          Character vector specifying the \link[=timezones]{time zone} \emph{Default:} \code{Sys.timezone()}
        }

        \item{\code{start_time}}{
          Time from which on the data should be considered. Formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or in another format automatically tried by \link[=as.POSIXct]{as.POSIX*}. \emph{Default:} \code{NULL}
        }


        \item{\code{end_time}}{
          Time up to which the data should be considered. Formatted as "\%Y-\%m-\%d \%H:\%M:\%OS" or in another format automatically tried by \link[=as.POSIXct]{as.POSIX*}. \emph{Default:} \code{NULL}
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
        \item{\code{...}}{
        Further arguments passed to \code{\link[=fread]{data.table::fread}}.
        }
      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

dir <- system.file("extdata", package = "triact")

my_triact$load_files(input = dir,
                     id_substring = c(1, 5),
                     timeFwdUpRight_cols = c(1, 2 ,3, 4),
                     skip = "*DATA")

# inspect imported data
head(my_triact$data)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-load_table"></a>}}
  \if{latex}{\out{\hypertarget{method-load_table}{}}}

  \subsection{Method \code{load_table()}}{
    Import acceleration data from a data.frame-like table (see \code{\link[=cows_5hz]{cows_5hz}} as an example). The table should contain the following columns:
      \tabular{llll}{
    \emph{colname} \tab \emph{type} \tab \emph{description} \cr
    id \tab Factor \tab unique id for the cow \cr
    time \tab POSIXct \tab timestamp \cr
    acc_fwd \tab numeric \tab acceleration from \emph{forward} axis (units: \emph{g}) \cr
    acc_up \tab numeric \tab acceleration from \emph{up} axis (units: \emph{g}) \cr
    acc_right \tab numeric \tab acceleration from \emph{right} axis (units: \emph{g}) \cr
  }
The accelerometer sampling frequency must be the same across all cows (id). No time gaps are allowed (within data of one id). One or two of the acceleration columns may be missing, but the possible analyses are then limited.

\emph{Important}: Make sure the accelerometer axes correctly represent body relative axes (forward, up, right). See Simmler & Brouwers (2023) for an illustration.


    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$load_table(table)
      }\if{html}{\out{</div>}}
    }
    \subsection{Usage (alternative syntax)}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$data <- table
      }\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{table}}{Data frame-like table containing the data to import. Must follow the requirements detailed in the description above.}
      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

# inspect imported data
head(my_triact$data)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-check_orientation"></a>}}
  \if{latex}{\out{\hypertarget{method-check_orientation}{}}}

  \subsection{Method \code{check_orientation()}}{
      Checks for each ID (unique identifier of the cow) whether the accelerometer may have been unintentionally mounted 180° rotated in the sagittal plane to the hind leg. If identified as such, the mathematical correction is applied, i.e. the forward and up axes are negated (multiplied by -1), i.e. the axes are mathematically rotated in order to comply with the orientation as specified when loading in the data.
      The check is \code{sum(acc_up > crit) < sum(acc_up < (-1 * crit))} with \code{crit = 0.5} by default.
    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$check_orientation(crit = 0.5, interactive = TRUE)}\if{html}{\out{</div>}}
    }
        \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{crit}}{
          Critical value used in the check according to the expression noted in the description above.  \emph{Default:} \code{0.5}
        }
        \item{\code{interactive}}{
                    A logical value that indicates whether the function should interactively prompts the user before applying the correction. \emph{Default:} \code{TRUE}
        }
      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$check_orientation()
      }
    }
  }


  %% ------------------------------
  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-add_lying"></a>}}
  \if{latex}{\out{\hypertarget{method-add_lying}{}}}

  \subsection{Method \code{add_lying()}}{
    Classify data into lying and standing and add results as column 'lying' to the ‘Triact’ object. Additionally, bouts (lying and standing) are uniquely numbered in column 'bout_id'.
    The simple rule-based algorithm is composed of three steps: In the first step, the \emph{up} acceleration is filtered to obtain the gravity component of the signal. In the second step, a threshold is used to classify the filtered acceleration values into lying and standing. Finally, in the third step, lying bouts shorter than a given minimum duration are reclassified as standing. The last step can be performed analogous for standing bouts, but is not recommended by default. See Simmler & Brouwers (2023) for a detailed discussion.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$add_lying(
      filter_method = "median",
      crit_lie = 0.5,
      minimum_duration_lying = 30,
      minimum_duration_standing = NULL,
      add_filtered = FALSE,
      ...)}\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{filter_method}}{
          Filter method to be applied to obtain the gravity component of the acceleration on the \emph{up} axis. Options are "median", for median filter, and "butter" for a bidirectional (zero-lag) Butterworth low-pass filter. Settings to the respective filter methods can be passed via the '...' argument (see below). \emph{Default:} \code{"median"}
        }
        \item{\code{crit_lie}}{
          Up-acceleration threshold for classifying into lying and standing. \emph{Default:} \code{0.5}
        }
        \item{\code{minimum_duration_lying}}{
          Minimum duration for lying bouts in seconds. Lying bouts shorter than this threshold are considered false and reclassified as standing. \emph{Default:} \code{30}
        }
        \item{\code{minimum_duration_standing}}{
        Minimum duration for standing bouts in seconds. Standing bouts shorter than this threshold are considered false and reclassified as lying.
           \emph{Default:} \code{NULL}
        }
        \item{\code{add_filtered}}{
        Logical value that indicates whether the filtered gravity component of the \emph{up} acceleration should be added to the 'Triact' object.
           \emph{Default:} \code{FALSE}
        }
        \item{\code{...}}{
        Further arguments passed to the corresponding filter method as specified with the \code{filter_method} argument. For filter method 'median' the argument \code{window_size} specifies the window size in seconds (\emph{default:} \code{10}). For filter method 'butter' the argument \code{cutoff} specifies the cutoff frequency for low-pass filtering in Hz (\emph{default:} \code{0.1}) and the argument \code{order} specifies the Butterworth filter order (\emph{default:} \code{1}).
        }

      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

# inspect result
head(my_triact$data)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-add_side"></a>}}
  \if{latex}{\out{\hypertarget{method-add_side}{}}}

  \subsection{Method \code{add_side()}}{
    Classify lying bouts into left and right lying side and add results as column 'side' to the 'Triact' object. For each lying bout, the median acceleration over the entire duration is calculated and compared to a threshold to classify the bout as 'L' (left) or 'R' (right) lying side. See Simmler & Brouwers (2023) for details.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$add_side(left_leg, crit_left = if(left_leg) -0.5 else 0.5)}\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{left_leg}}{Logical indicating whether the accelerometers were attached to the left hind leg (\code{TRUE}) or to the right hind leg (\code{FALSE}). This information is used to choose the default value for \code{crit_left}. It is ignored if \code{crit_left} is specified by the user.}
        \item{\code{crit_left}}{Right-acceleration threshold for classifying lying on left versus right side. \emph{Default:} \code{-0.5} if \code{left_leg} is \code{TRUE}, else \code{0.5}}
      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

my_triact$add_side(left_leg = TRUE)

# inspect result
head(my_triact$data)
      }
    }
  }



  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-add_activity"></a>}}
  \if{latex}{\out{\hypertarget{method-add_activity}{}}}

  \subsection{Method \code{add_activity()}}{
  Calculate proxies for the physical activity level. By default, the L2 norm of the dynamic body acceleration (DBA) vector is calculated. The corresponding L1 norm is optionally available. Also, the L1 and L2 norms of the jerk vector can be calculated. By default, all activity values during lying bouts are 'adjusted' to zero, i.e., periods when cows are lying are considered as 'inactive' by definition. See Simmler & Brouwers (2023) for details.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$add_activity(
      dynamic_measure = "dba",
      norm = "L2",
      adjust = TRUE,
      filter_method = "median",
      keep_dynamic_measure = FALSE,
      ...)}\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{dynamic_measure}}{
            Type of dynamic measure to base the activity proxy on. Options are \code{"dba"}, for dynamic body acceleration, and \code{"jerk"}, for the jerk vector. One or both can be provided, e.g., \code{"dba"} or \code{c("dba", "jerk")}. \emph{Default:} \code{"dba"}
        }
                \item{\code{norm}}{
            The type of norm to be calculated. Options are \code{"L1"} and \code{"L2"}. One or both can be provided, e.g., \code{"L1"} or \code{c("L1", "L2")}. \emph{Default:} \code{"L2"}
        }
                \item{\code{adjust}}{
            A logical value that indicates whether the proxies for physical activity should be 'adjusted' to 0 during lying bouts, i.e., whether cows should be considered as inactive by definition when lying. \emph{Default:} \code{TRUE}
            }

                \item{\code{filter_method}}{
                Filter method to be used to determine the gravity component subtracted from the raw acceleration to obtain the dynamic body acceleration. Options are "median", for median filter, and "butter" for a bidirectional (zero-lag) Butterworth low-pass filter. Settings to the respective filter methods can be passed via the '...' argument (see below). \emph{Default:} \code{"median"}
        }
                \item{\code{keep_dynamic_measure}}{
            A logical value that indicates whether the intermediate data, being the dynamic body acceleration vector and/or the jerk vector, should be added to the 'Triact' object. \emph{Default:} \code{FALSE}
        }
                \item{\code{...}}{
        Further arguments passed to the corresponding filter method as specified with the \code{filter_method} argument. For filter method 'median' the argument \code{window_size} specifies the window size in seconds (\emph{default:} \code{10}). For filter method 'butter' the argument \code{cutoff} specifies the cutoff frequency for low-pass filtering in Hz (\emph{default:} \code{0.1}) and argument \code{order} specifies the Butterworth filter order (\emph{default:} \code{1}).
        }
      }
      \if{html}{\out{</div>}}
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

my_triact$add_activity()

# inspect result
head(my_triact$data)
      }
    }
  }


  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-summarize_intervals"></a>}}
  \if{latex}{\out{\hypertarget{method-summarize_intervals}{}}}

  \subsection{Method \code{summarize_intervals()}}{
    Summarizes the data  in the 'Triact' object, activity and lying behaviour, by regular intervals. The information contained in the output table depends on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods. With \code{bout = TRUE} information on bouts per interval (number and mean bout duration) will be returned additionally. With \code{side = TRUE} summarized information such lying duration is additionally provided separately for the lying side (left/right). For measures such as the number of lying bouts or mean lying bout duration, a weighted mean ('wMean...') is calculated with the weights being the proportion of the individual bout overlapping with the respective interval. See Simmler & Brouwers (2023) for details.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$summarize_intervals(
      interval = "hour",
      lag_in_s = 0,
      duration_units = "mins",
      bouts = FALSE,
      side = FALSE,
      calc_for_incomplete = FALSE)}\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{interval}}{Character string specifying the intervals to be analyzed. Any unique English abbreviation valid for the 'unit' argument of \code{\link[=floor_date]{lubridate::floor_date}} is allowed, e.g., \code{"hour"}, \code{"min"}, \code{"10 mins"}, and \code{"0.5 hours"}. \emph{Default:} \code{"hour"}}
        \item{\code{lag_in_s}}{Lag in seconds with respect to the full hour or full day. \emph{Default:} \code{0}}
        \item{\code{duration_units}}{Unit in which durations should be returned. Options are \code{"secs"}, \code{"mins"} and \code{"hours"}. \emph{Default:} \code{"mins"}}
        \item{\code{bouts}}{Logical indicating whether information on bouts should be additionally summarized. \emph{Default:} \code{FALSE}}
        \item{\code{side}}{Logical indicating whether lying side should be considered in the summary. \emph{Default:} \code{FALSE}}
        \item{\code{calc_for_incomplete}}{Logical indicating whether a complete summary should also be returned for the incompletely observed intervals (first and last interval for each cow) and for any parameter using information of incompletely observed bouts (first and last bout for each cow). Please note that these are ill-defined. \emph{Default:} \code{FALSE}}
      }
    }
    \if{html}{\out{</div>}}

    \subsection{Returns}{
      A table with summaries by interval (rows). See \link{interval_summary} for a complete list.
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

int_summary <- my_triact$summarize_intervals()

# inspect result
head(int_summary)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-summarize_bouts"></a>}}
  \if{latex}{\out{\hypertarget{method-summarize_bouts}{}}}

  \subsection{Method \code{summarize_bouts()}}{
    Summarizes the data in the 'Triact' object, activity and lying behaviour, by lying/standing bouts. The information contained in the output table depends on the analyses you added to the 'Triact' object using the $add_activity(), $add_lying(), and $add_side() methods.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$summarize_bouts(
      bout_type = "both",
      duration_units = "mins",
      calc_for_incomplete = FALSE)}\if{html}{\out{</div>}}
    }

    \subsection{Arguments}{
      \if{html}{\out{<div class="arguments">}}
      \describe{
        \item{\code{bout_type}}{ Type of bout to be considered. Options are \code{"both"}, \code{"lying"}, and \code{"standing"}. \emph{Default:} \code{"both"}}
        \item{\code{duration_units}}{Units in which durations should be returned. Options are \code{"secs"}, \code{"mins"}, and \code{"hours"}. \emph{Default:} \code{"mins"}}
        \item{\code{calc_for_incomplete}}{Logical indicating whether a complete summary should also be returned for the incompletely observed bouts (first and last bout for each cow). Please note that these are ill-defined. \emph{Default:} \code{FALSE}}
      }
    }
    \if{html}{\out{</div>}}

    \subsection{Returns}{
    A table with summaries by bout (rows).  See \link{bout_summary} for a complete list.
    }
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

bouts_summary <- my_triact$summarize_bouts()

# inspect result
head(bouts_summary)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-extract_liedown"></a>}}
  \if{latex}{\out{\hypertarget{method-extract_liedown}{}}}

  \subsection{Method \code{extract_liedown()}}{
    Extracts data associated with liedown events (standing-to-lying transitions). Operates in two modes, see \emph{Returns} section.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$extract_liedown(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
    }

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
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

l_downs <- my_triact$extract_liedown()

# inspect result
print(l_downs)
      }
    }
  }

  %% ------------------------------

  \if{html}{\out{<hr>}}
  \if{html}{\out{<a id="method-extract_standup"></a>}}
  \if{latex}{\out{\hypertarget{method-extract_standup}{}}}

  \subsection{Method \code{extract_standup()}}{
    Extracts data associated with standup events (lying-to-standing transitions). Operates in two modes, see \emph{Returns} section.

    \subsection{Usage}{
      \if{html}{\out{<div class="r">}}\preformatted{Triact$extract_standup(sec_before = 0, sec_after = 0)}\if{html}{\out{</div>}}
    }

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
    \subsection{Examples}{
      \preformatted{
# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

my_triact$add_lying()

st_ups <- my_triact$extract_standup()

# inspect result
print(st_ups)
      }
    }
  }
}

  %% ------------------------------

    \references{
Simmler. M., Brouwers S., 2023. Triact package for R: Analyzing the lying behavior of cows from accelerometer data. \href{https://gitlab.com/AgroSimi/triact_manuscript}{pdf}

}

    \examples{

## Please read the "introduction" vignette for more detailed examples

## -------------------------
# method '$new'
## -------------------------

# create a Triact object
my_triact <- Triact$new()

## -------------------------
# method '$load_files'
## -------------------------

\dontrun{
# create a Triact object
my_triact <- Triact$new()

dir <- system.file("extdata", package = "triact")

my_triact$load_files(input = dir,
                     id_substring = c(1, 5),
                     timeFwdUpRight_cols = c(1, 2 ,3, 4),
                     skip = "*DATA")

# inspect imported data
head(my_triact$data)
}

## -------------------------
# method 'load_table'
## -------------------------

# create a Triact object
my_triact <- Triact$new()

my_triact$load_table(cows_5hz)

# inspect imported data
head(my_triact$data)

## -------------------------
# method 'check_orientation'
## -------------------------

my_triact$check_orientation()

## -------------------------
# method 'add_lying'
## -------------------------

my_triact$add_lying()

# inspect result
head(my_triact$data)

## -------------------------
# method 'add_side'
## -------------------------

my_triact$add_side(left_leg = TRUE)

# inspect result
head(my_triact$data)

## -------------------------
# method 'add_activity'
## -------------------------

my_triact$add_activity()

# inspect result
head(my_triact$data)

## -------------------------
# method 'summarize_intervals'
## -------------------------

int_summary <- my_triact$summarize_intervals()

# inspect result
head(int_summary)

## -------------------------
# method 'summarize_bouts'
## -------------------------

bouts_summary <- my_triact$summarize_bouts()

# inspect result
head(bouts_summary)

## -------------------------
# method 'extract_liedown'
## -------------------------

l_downs <- my_triact$extract_liedown()

# inspect result
print(l_downs)

## -------------------------
# method 'extract_standup'
## -------------------------

st_ups <- my_triact$extract_standup()

# inspect result
print(st_ups)

}

