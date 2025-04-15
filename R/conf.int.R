



#' @method print conf.int
#' @export
print.conf.int <- function(x, digits = getOption('digits'), ...) {
  # see inside ?stats:::print.htest
  cat(format(100 * attr(x, which = 'conf.level', exact = TRUE)), 
      ' percent confidence interval:\n', 
      ' ', 
      paste(format(x[1:2], digits = digits), collapse = ' '), 
      '\n', 
      sep = '')
}

