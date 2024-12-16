

#' @title Summary Information of Cohen's \link[vcd]{Kappa}
#' 
#' @param model a \link[vcd]{Kappa} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' # ?vcd::Kappa
#' library(vcd)
#' data('SexualFun', package = 'vcd')
#' cat(Sprintf.Kappa(Kappa(SexualFun)))
#' @keywords internal
#' @importFrom vcd Kappa
#' @importFrom cli style_bold col_magenta
#' @export
Sprintf.Kappa <- function(model, ...) {
  ci_ <- confint(model) # vcd:::confint.Kappa
  x <- model$Weighted['value']
  ci <- ci_[rownames(ci_) == 'Weighted',]
  sprintf(
    fmt = 'Cohen\'s Agreement \u03ba = %.2f, %s, 95%% CI (%.2f, %.2f)', 
    # this is aymptotic/approximate
    x, 
    style_bold(col_magenta(as.character.factor(cut.default(
      # \url{https://en.wikipedia.org/wiki/Cohen\%27s_kappa}
      x = x, breaks = c(-Inf, 0, .2, .4, .6, .8, 1), 
      labels = c('no', 'slight', 'fair', 'moderate', 'substantial', 'almost perfect'), 
      right = TRUE, include.lowest = TRUE
    )))), 
    ci[1L], ci[2L])
}