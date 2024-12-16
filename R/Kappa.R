

#' @title Summary Information of Cohen's \link[vcd]{Kappa}
#' 
#' @param model a \link[vcd]{Kappa} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' # ?vcd::Kappa
#' library(vcd)
#' (jobsat1 = xtabs(Freq ~ supervisor + own, data = JobSatisfaction))
#' (jobsat2 = xtabs(Freq ~ management + own, data = JobSatisfaction))
#' cat(Sprintf.Kappa(Kappa(jobsat1)))
#' cat(Sprintf.Kappa(Kappa(jobsat2)))
#' @keywords internal
#' @importFrom cli style_bold col_magenta
#' @export
Sprintf.Kappa <- function(model, ...) {
  # this is aymptotic/approximate
  ci_ <- confint(model) # vcd:::confint.Kappa
  x <- model$Weighted['value']
  ci <- ci_[rownames(ci_) == 'Weighted',]
  
  id <- .bincode(x = x, breaks = c(-Inf, 0, .2, .4, .6, .8, 1), right = TRUE, include.lowest = TRUE)
  txt <- c('no', 'slight', 'fair', 'moderate', 'substantial', 'almost perfect')[id]
  
  sprintf(
    fmt = 'Cohen\'s Agreement \u03ba = %.2f, %s, 95%% CI (%.2f, %.2f)', 
    x, 
    style_bold(col_magenta(txt)), 
    ci[1L], ci[2L])
}