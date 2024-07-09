
#' @title Chapter 2: Descriptive Statistics
#' 
#' @description 
#' 
#' Functions and examples for Chapter 2, \emph{Descriptive Statistics}.
#' 
#' @param x \link[base]{numeric} vector, the observations. 
#' In function [print_freqs], this argument can also be a \link[base]{factor}
#' 
#' @param breaks \link[base]{numeric} vector, see \link[base]{cut.default}
#' 
#' @param include.lowest \link[base]{logical} scalar, default `TRUE`. See \link[base]{cut.default}
#' 
#' @param right \link[base]{logical} scalar, see \link[base]{cut.default}
#' 
#' @param na.rm \link[base]{logical} scalar, whether to remove the missing observations (default `TRUE`)
#' 
#' @details 
#' 
#' Function [print_freqs] prints the (relative) frequencies and cumulative (relative) frequencies, from 
#' a numeric input vector, specified interval breaks as well as open/close status of the ends of the intervals.
#' 
#' Function [print_stats] prints the simple statistics of the input observations, such as sample size,
#' mean, median, (smallest) mode, variance, standard deviation, 
#' coefficient of variation (if all observations are non-negative),
#' quartiles, inter-quartile range (IQR), range, skewness and kurtosis.  A histogram is also printed. 
#' 
#' @return 
#' 
#' Function [print_freqs] returns a \linkS4class{freqs} object, for which
#' a \link[methods]{show} method is defined.
#' 
#' Function [print_stats] does not have a returned value.
#' 
#' @seealso \link[base]{cut.default} \link[base]{table} \link[base]{cumsum}
#' \link[base]{mean.default} \link[stats]{median.default} \link[pracma]{Mode} 
#' \link[stats]{var} \link[stats]{sd} \link[stats]{quantile}
#' \link[e1071]{skewness} \link[e1071]{kurtosis}
#' 
#' 
#' @example inst/extexample/Chapter2.R
#' 
#' @name Chapter02
#' @importFrom e1071 skewness kurtosis
#' @importFrom pracma Mode
#' @importFrom stats median.default quantile sd var
#' @export
print_stats <- function(x, na.rm = TRUE) {
  nm <- deparse(substitute(x))
  cat('\nSummary Statistics of', sQuote(nm), '\n\n')
  cat(sprintf('Number of observations = %d\n', length(if (na.rm) x[!is.na(x)] else x)))
  cat(sprintf('mean = %.2f\n', mean.default(x, na.rm = na.rm)))
  cat(sprintf('median = %.2f\n', median.default(x, na.rm = na.rm)))
  cat(sprintf('(smallest) mode = %.2f\n', Mode(x)[1L]))
  cat(sprintf('variance = %.2f\n', var(x, na.rm = na.rm)))
  cat(sprintf('standard deviation = %.2f\n', sd(x, na.rm = na.rm)))
  if (all(x >= 0, na.rm = TRUE)) {
    cat(sprintf('coefficient of variation = %.1f%%\n', 1e2 * sd(x, na.rm = na.rm) / mean.default(x, na.rm = na.rm)))
  } else cat('(coefficient of variation only applicable to all-non-negative observations)\n')
  Q <- quantile(x, probs = c(.25, .5, .75), na.rm = na.rm)
  cat(sprintf('Quartiles: Q1 = %.1f, Q2 = %.1f, Q3 = %.1f\n', Q[1L], Q[2L], Q[3L]))
  cat(sprintf('IQR = %.1f\n', Q[3L] - Q[1L]))
  cat(sprintf('range = %.1f (%.1f ~ %.1f)\n', diff.default(range.default(x, na.rm = na.rm)), min(x, na.rm = na.rm), max(x, na.rm = na.rm)))
  cat(sprintf('skewness = %.3f\n', skewness(x, na.rm = na.rm)))
  cat(sprintf('kurtosis = %.3f\n', kurtosis(x, na.rm = na.rm)))
  cat('\n')
  # to avoid import \pkg{ggplot2}
  #p <- ggplot() + geom_histogram(mapping = aes(x = x), bins = 30L, colour = 'white') + 
  #  labs(x = nm, y = NULL) +
  #  theme_bw()
  #print(p)
  return(invisible())
}


#' @rdname Chapter02
#' @importFrom methods new
#' @export
print_freqs <- function(x, breaks, include.lowest = TRUE, right = TRUE) {
  data.name <- deparse1(substitute(x))
  object <- if (is.factor(x)) {
    x 
  } else if (is.integer(x) && missing(breaks)) {
    factor(x)
  } else {
    if (!is.numeric(x)) stop(sQuote(data.name), ' must be numeric')
    # ?base::cut.default will stop on !is.numeric(x), but the error message is not informative enough
    cut.default(x, breaks = breaks, include.lowest = include.lowest, right = right, ordered_result = TRUE)
  }
  new(Class = 'freqs', c(table(object)), data.name = data.name)
}


# same as in \pkg{tzh}
#' @title S4 Class \linkS4class{freqs}
#' 
#' @slot .Data \link[base]{integer} \link[base]{vector}, frequency counts
#' 
#' @slot data.name \link[base]{character} integer, name of the data, only used in output
#' 
#' @keywords internal
#' @importFrom methods setClass
#' @export
setClass(Class = 'freqs', contains = 'integer', slots = c(
  data.name = 'character'
), validity = function(object) {
  if (!length(x <- unclass(object)) || anyNA(x) || any(x < 0L)) stop('counts must be all-non-missing integers')
  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('illegal category names')
})




#' @title Show \linkS4class{freqs} Object
#' 
#' @description Show \linkS4class{freqs} object
#' 
#' @param object an \linkS4class{freqs} object
#' 
#' @return 
#' The \link[methods]{show} method for \linkS4class{freqs} object 
#' does not have a returned value.
#' 
#' @keywords internal
#' @importFrom methods setMethod show signature
#' @export
setMethod(f = show, signature = signature(object = 'freqs'), definition = function(object) {
  freq <- unclass(object)
  cfreq <- cumsum(freq)
  rev_cfreq <- rev.default(cumsum(rev.default(freq)))
  n <- sum(freq)
  ret <- cbind(
    'Frequency' = sprintf(fmt = '%d (%.2f%%)', freq, 100 * freq/n), 
    'Cummulative Freq' = sprintf(fmt = '%d (%.2f%%)', cfreq, 100 * cfreq/n),
    'Reversed Cumm Freq' = sprintf(fmt = '%d (%.2f%%)', rev_cfreq, 100 * rev_cfreq/n)
  )
  rownames(ret) <- names(freq)
  names(dimnames(ret)) <- c(object@data.name, 'Counts (%)')
  print.noquote(noquote(ret, right = TRUE))
})

