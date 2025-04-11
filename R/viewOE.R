

#' @title View Observed and Expected Frequency Table
#' 
#' @param x returned object of function \link[stats]{chisq.test}
#' 
#' @return
#' Function [viewOE()] returns a \link[flextable]{flextable} of 
#' observed and expected frequencies, as well as 
#' the category-wise \eqn{\chi^2} statistics.
#' 
#' @keywords internal
#' @importFrom flextable flextable autofit align set_header_labels
#' @export
viewOE <- function(x) {
  
  O <- x[['observed']]
  if (!length(O) || !is.integer(O) || anyNA(O)) stop('observed data must be non-negative integer')
  
  E <- x[['expected']]
  
  data.frame(
    nm = O |> names(),
    o = O,
    e = sprintf(fmt = '%.2f (%.2f%%)', E, 1e2*E/sum(O)),
    chisq = ((O-E)^2/E) |> sprintf(fmt = '%.3f')
  ) |> 
    flextable() |>
    set_header_labels(
      nm = ' ',
      o = 'Observed\nFrequency', 
      e = 'Expected\nFrequency (%)', 
      chisq = '(O-E)^2/E'
    ) |> 
    autofit() |>
    align(align = 'right', part = 'all')
  
}



#' @title Additional Restriction on \link[stats]{chisq.test}
#' 
#' @param x returned object of function \link[stats]{chisq.test}
#' 
#' @param restriction \link[base]{integer} scalar, number of additional restrictions
#' 
#' @keywords internal
#' @export
update_df <- function(x, restriction) {
  
  if (length(restriction) != 1L || !is.integer(restriction) || is.na(restriction) || restriction <= 0L || restriction > x$parameter) stop('illegal additional `restriction`')
  
  x$parameter[] <- x$parameter - restriction
  
  x$p.value <- x$statistic |> 
    pchisq(df = x$parameter, lower.tail = FALSE) |>
    unname()
  
  x$method <- paste(x$method, sprintf(fmt = 'Additional Restriction: %d', restriction), sep = '\n\n')
  
  return(x)
  
}






