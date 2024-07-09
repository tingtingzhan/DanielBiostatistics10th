

#' @title Chapter 1: Introduction to Biostatistics
#' 
#' @description 
#' 
#' Functions and examples for Chapter 1, \emph{Introduction to Biostatistics}.
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param size positive \link[base]{integer} scalar, number of rows to be selected
#' 
#' @param replace \link[base]{logical} scalar, whether sampling should be with replacement (default `FALSE`)
#' 
#' @param prob \link[base]{numeric} vector of probability weights for each row of input `x` being sampled.
#' Default `NULL` indicates simple random sampling
#' 
#' @return 
#' 
#' Function [sampleRow] returns a \link[base]{data.frame}, a simple random sample from the input.
#' 
#' @seealso \link[base]{sample.int}
#' 
#' @example inst/extexample/Chapter1.R
#' 
#' @name Chapter01
#' @export
sampleRow <- function(x, size, replace = FALSE, prob = NULL) {
  if (!is.data.frame(x)) stop('input must be data.frame')
  if (!length(nr <- nrow(x))) stop('input data.frame cannot be of 0-row')
  message('A random sample of ', size, ' rows')
  x[sample.int(n = nr, size = size, replace = replace, prob = prob), ]
}

