

#' @title Chapter 12: \eqn{\chi^2} Distribution and the Analysis of Frequencies
#' 
#' @description 
#' 
#' Functions for Chapter 12, \emph{The Chi-Square Distribution and the Analysis of Frequencies}.
#' 
#' @param O \link[base]{integer} vector, observed counts
#' 
#' @param prob \link[base]{numeric} vector, anticipated probability.
#' If missing (default), an uniform distribution across all categories are used.
#' 
#' @return
#' 
#' Function [print_OE] prints a table with observed and expected frequencies, as well as 
#' the category-wise \eqn{\chi^2} statistics.  
#' A \link[base]{double} vector of the category-wise \eqn{\chi^2} statistics is returned invisibly.
#' 
#' @example inst/extexample/Chapter12.R 
#' 
#' @name Chapter12
#' @export
print_OE <- function(O, prob) {
  if (!is.integer(O) || (nO <- length(O)) <= 1L || anyNA(O) || any(O < 0L)) stop('observed data must be non-negative integer')
  if (missing(prob)) {
    prob <- rep(1/nO, times = nO)
  } else {
    if (!is.numeric(prob) || length(prob) != nO || anyNA(prob) || any(prob < 0)) stop('prob must be non-negative numerics')
    prob <- prob / sum(prob)
  }
  E <- sum(O) * prob
  chisq <- (O - E)^2 / E
  ret <- cbind(
    'Observed Freq' = O,
    'Expected Freq (%)' = sprintf(fmt = '%.2f (%.2f%%)', E, 1e2*prob),
    '(O-E)^2/E' = sprintf(fmt = '%.3f', chisq)
  )
  print.noquote(noquote(ret, right = TRUE))
  cat(sprintf(fmt = '\nSum (O-E)^2/E = %.2f\n', sum(chisq)))
  return(invisible(chisq))
}
