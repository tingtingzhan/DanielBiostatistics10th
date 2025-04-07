
#' @title Print Summary Statistics
#' 
#' @description 
#' 
#' Print summary statistics.
#' 
#' @param x \link[base]{numeric} vector, the observations. 
#' 
#' @param na.rm \link[base]{logical} scalar, whether to remove the missing observations, default `TRUE`.
#' 
#' @details 
#' Function [viewStats()] prints the simple statistics of the input observations, such as sample size,
#' mean, median, (smallest) mode, variance, standard deviation, 
#' coefficient of variation (if all observations are non-negative),
#' quartiles, inter-quartile range (IQR), range, skewness and kurtosis.  A histogram is also printed. 
#' 
#' @return 
#' Function [viewStats()] does not have a returned value.
#' 
#' @keywords internal
#' @importFrom e1071 skewness kurtosis
#' @importFrom pracma Mode
#' @importFrom stats median.default quantile sd var
#' @export
viewStats <- function(x, na.rm = TRUE) {
  nm <- deparse(substitute(x))
  cat('\nSummary Statistics of', sQuote(nm), '\n\n')
  sprintf('Number of observations = %d\n', length(if (na.rm) x[!is.na(x)] else x)) |> cat()
  x |> mean.default(na.rm = na.rm) |> sprintf(fmt = 'mean = %.2f\n') |> cat()
  x |> median.default(na.rm = na.rm) |> sprintf(fmt = 'median = %.2f\n') |> cat()
  (Mode(x)[1L]) |> sprintf(fmt = '(smallest) mode = %.2f\n') |> cat()
  x |> var(na.rm = na.rm) |> sprintf(fmt = 'variance = %.2f\n') |> cat()
  x |> sd(na.rm = na.rm) |> sprintf(fmt = 'standard deviation = %.2f\n') |> cat()
  if (all(x >= 0, na.rm = TRUE)) {
    sprintf(fmt = 'coefficient of variation = %.1f%%\n', 1e2 * sd(x, na.rm = na.rm) / mean.default(x, na.rm = na.rm)) |> cat()
  } else cat('(coefficient of variation only applicable to all-non-negative observations)\n')
  Q <- quantile(x, probs = c(.25, .5, .75), na.rm = na.rm)
  sprintf('quartiles: Q1 = %.1f, Q2 = %.1f, Q3 = %.1f\n', Q[1L], Q[2L], Q[3L]) |> cat()
  sprintf('IQR = %.1f\n', Q[3L] - Q[1L]) |> cat()
  sprintf('range = %.1f (%.1f ~ %.1f)\n', diff.default(range.default(x, na.rm = na.rm)), min(x, na.rm = na.rm), max(x, na.rm = na.rm)) |> cat()
  x |> skewness(na.rm = na.rm) |> sprintf(fmt = 'skewness = %.3f\n') |> cat()
  kurtosis(x, na.rm = na.rm) |> sprintf(fmt = 'kurtosis = %.3f\n') |> cat()
  cat('\n')
  return(invisible())
}





#' @title View Frequency Table
#' 
#' @param x an \link[base]{ordered} \link[base]{factor}
#' 
#' @details
#' Function [viewFreq()] shows the (relative) frequencies and cumulative (relative) frequencies.
#' 
#' @returns 
#' Function [viewFreq()] returns a \link[flextable]{flextable}
#' 
#' @keywords internal
#' @importFrom flextable flextable autofit
#' @export
viewFreq <- function(x) {
  
  if (!inherits(x, what = 'ordered')) stop('input must be ordered factor')
  f <- x |> table() # frequency
  cf <- cumsum(f) # cumulative frequency
  rcf <- f |> rev.default() |> cumsum() |> rev.default() # reversed cumulative frequency
  n <- sum(f)
  
  data.frame(
    ' ' = names(f),
    'Frequency' = sprintf(fmt = '%d (%.2f%%)', f, 100 * f/n), 
    'Cummulative Frequency' = sprintf(fmt = '%d (%.2f%%)', cf, 100 * cf/n),
    'Reversed Cummulative Frequency' = sprintf(fmt = '%d (%.2f%%)', rcf, 100 * rcf/n),
    check.names = FALSE
  ) |> 
    flextable() |>
    autofit()

}

