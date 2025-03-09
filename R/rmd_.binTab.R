


#' @title Create R Markdown Script for \link[DanielBiostatistics10th]{binTab} Object
#' 
#' @description
#' Create R Markdown Script for \link[DanielBiostatistics10th]{binTab} object.
#' 
#' @param x a \link[DanielBiostatistics10th]{binTab}
#' 
#' @param xnm \link[base]{language} or \link[base]{character} scalar, call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_.binTab()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @name rmd_binTab
#' @export
rmd_.binTab <- function(x, xnm, ...) {
  c(
    Sprintf.binTab(x),
    '```{r results = \'asis\'}', 
    sprintf(fmt = 'as_flextable(%s)', xnm),
    '```', 
    '```{r comment = NA}', 
    paste0('print.binTab(', xnm, ', print_flextable = TRUE)'), # how to put in `prevalence` here??
    '```'
  )
}


#' @rdname rmd_binTab
#' @export
Sprintf.binTab <- function(x) {
  dnm <- dimnames(x)
  sprintf(fmt = 'Sensitivity, specificity and predictive values, as well as their 95%% exact confidence intervals, are provided for the 2-by-2 table of `%s` and `%s`.',
          names(dnm)[1L], names(dnm)[2L])
}





