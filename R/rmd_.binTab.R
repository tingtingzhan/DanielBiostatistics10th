


#' @title Description for \link[DanielBiostatistics10th]{binTab} Object
#' 
#' @description
#' description for \link[DanielBiostatistics10th]{binTab} object.
#' 
#' @param x a \link[DanielBiostatistics10th]{binTab}
#' 
#' @returns 
#' Function [Sprintf.binTab()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
Sprintf.binTab <- function(x) {
  dnm <- dimnames(x)
  sprintf(fmt = 'Sensitivity, specificity and predictive values, as well as their 95%% exact confidence intervals, are provided for the 2-by-2 table of `%s` and `%s`.',
          names(dnm)[1L], names(dnm)[2L])
}





