
# in English
# 'Boolean' indicates 0/1; https://en.wikipedia.org/wiki/Boolean_data_type
# 'binary' indicates base-2 numerical system, i.e., er-jin-zhi; https://en.wikipedia.org/wiki/Binary_number


#' @title Boolean Test-&-Disease or Risk-&-Disease Table 
#' 
#' @description
#' To create a Boolean test-&-disease or risk-&-disease table.
#' 
#' @param x (an R object convertible to a) \eqn{2\times 2} \link[base]{integer} \link[base]{matrix}, 
#' contingency table of two Boolean variables.
#' The endpoint (i.e., disease) is on rows and the test/risk on columns.
#' 
#' @param formula \link[stats]{formula} in the fashion of `~disease+test` or `~disease+risk`, see function \link[stats]{xtabs}
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @details
#' 
#' Function [binTab] creates a \eqn{2\times 2} test-&-disease contingency table with layout
#' \tabular{lcc}{
#'  \tab Test (\eqn{-}) \tab Test (\eqn{+}) \cr
#' Disease (\eqn{-}) \tab \eqn{x_{--}} \tab \eqn{x_{-+}} \cr
#' Disease (\eqn{+}) \tab \eqn{x_{+-}} \tab \eqn{x_{++}} \cr
#' }
#' or a \eqn{2\times 2} risk-&-disease contingency table with layout
#' \tabular{lcc}{
#'  \tab Risk Factor (\eqn{-}) \tab Risk Factor (\eqn{+}) \cr
#' Disease (\eqn{-}) \tab \eqn{x_{--}} \tab \eqn{x_{-+}} \cr
#' Disease (\eqn{+}) \tab \eqn{x_{+-}} \tab \eqn{x_{++}} \cr
#' }
#' The endpoint (i.e., disease) must be on the rows and the test/risk on the columns.
#' 
#' @returns 
#' Function [binTab] returns a two-by-two \link[base]{integer} \link[base]{matrix}.
#' 
#' @seealso 
#' Function `caret::confusionMatrix` does not provide confidence intervals of 
#' sensitivity, specificity, etc.
#' 
#' @examples 
#' binTab(matrix(c(7L, 3L, 8L, 6L), nrow = 2L))
#' binTab(matrix(c(7L, 3L, 8L, 6L), nrow = 2L, dimnames = list(X = c('a','b'), NULL)))
#' binTab(~ (mag < 4.5) + (depth > 400), data = quakes)
#' @keywords internal
#' @importFrom stats setNames
#' @name binTab
#' @export
binTab <- function(x, ...) UseMethod('binTab')

#' @rdname binTab
#' @export binTab.matrix
#' @export
binTab.matrix <- function(x, ...) {
  if (!is.matrix(x) || (typeof(x) != 'integer') || any(dim(x) != 2L)) stop('input must be 2*2 integer matrix')
  
  # nm1 <- c('(+)', '(-)') # OLD!!!
  nm1 <- c('(-)', '(+)') # c(0,1) # more intuitive!!
  nm2 <- c('Endpoint', 'Test/Risk')
  
  if (!length(dnm <- dimnames(x))) {
    dimnames(x) <- setNames(list(nm1, nm1), nm = nm2)
    class(x) <- c('binTab', class(x))
    return(x)
  } 
  
  if (!length(names(dnm))) {
    names(dnm) <- nm2 
  } else {
    names(dnm)[1L] <- if (!nzchar(names(dnm)[1L]) || (names(dnm)[1L] == 'Endpoint')) 'Endpoint' else trimws(paste0(names(dnm)[1L], ' [Endpoint]'))
    names(dnm)[2L] <- if (!nzchar(names(dnm)[2L]) || (names(dnm)[2L] == 'Test/Risk')) 'Test/Risk' else trimws(paste0(names(dnm)[2L], ' [Test/Risk]'))
  }
  
  if (!length(dnm[[1L]])) dnm[[1L]] <- nm1 else {
    if (!all(nzchar(dnm[[1L]]))) stop('do not allow zchar rownames')
    if (!(dnm[[1L]][1L] %in% c('FALSE', '(-)'))) dnm[[1L]][1L] <- paste(dnm[[1L]][1L], '(-)')
    if (!(dnm[[1L]][2L] %in% c('TRUE', '(+)'))) dnm[[1L]][2L] <- paste(dnm[[1L]][2L], '(+)')
  }
  if (!length(dnm[[2L]])) dnm[[2L]] <- nm1 else {
    if (!all(nzchar(dnm[[2L]]))) stop('do not allow zchar rownames')
    if (!(dnm[[2L]][1L] %in% c('FALSE', '(-)'))) dnm[[2L]][1L] <- paste(dnm[[2L]][1L], '(-)')
    if (!(dnm[[2L]][2L] %in% c('TRUE', '(+)'))) dnm[[2L]][2L] <- paste(dnm[[2L]][2L], '(+)')
  }
  dimnames(x) <- dnm
  class(x) <- c('binTab', class(x))
  return(x)
}

#' @rdname binTab
#' @export binTab.table
#' @export
binTab.table <- binTab.matrix

#' @rdname binTab
#' @importFrom stats na.omit xtabs
#' @export binTab.formula
#' @export
binTab.formula <- function(formula, data, ...) {
  ret <- y0 <- xtabs(formula, data = data, na.action = na.omit)
  if (any(dim(y0) != 2L)) stop('cannot convert to binTab')
  return(binTab(ret))
}








#' @title Print Boolean Test-&-Disease and/or Risk-&-Disease Table 
#' 
#' @description 
#' Print Boolean test-&-disease and/or risk-&-disease table.
#' 
#' @param x a [binTab]
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar or \link[base]{vector}, prevalence of disease
#' 
#' @param ansi \link[base]{logical} scalar, whether to allow ANSI escapes.
#' ANSI escapes are rendered beautifully in RStudio console, but not in R vanilla GUI, nor in package \CRANpkg{rmarkdown}.
#' 
#' @param ... potential parameters, currently not in use 
#' 
#' @details
#' 
#' Function [print.binTab] prints the diagnostic test characteristics, 
#' e.g., sensitivity, specificity, predictive values, and diagnostic accuracy,
#' together with their \eqn{95\%} Clopper-Pearson exact confidence intervals.
#' 
#' @returns 
#' 
#' Function [print.binTab] does not have a returned value.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Diagnostic_odds_ratio}
#' 
#' @examples 
#' (x = array(c(95L, 10L, 31L, 82L), dim = c(2L, 2L)))
#' binTab(x)
#' print(binTab(x), prevalence = c(.0001, .001, .01))
#' @keywords internal
#' @importFrom cli ansi_strip
#' @importFrom stats binom.test pnorm qnorm confint
#' @importFrom vcd Kappa
#' @export print.binTab
#' @export
print.binTab <- function(
    x, 
    prevalence, 
    ansi = identical(.Platform$GUI, 'RStudio'), 
    ...
) {
  
  print.default(addmargins(unclass(x)))
  cat('\n')
  
  x11 <- x[2L,2L] # (+,+)
  x00 <- x[1L,1L] # (-,-)
  xr <- .rowSums(x, m = 2L, n = 2L) # Disease (-) and (+)
  xc <- .colSums(x, m = 2L, n = 2L) # Test (-) and (+)
  
  foo <- if (ansi) identity else ansi_strip
  
  sens <- x11 / xr[2L]
  spec <- x00 / xr[1L]
  cat(do.call(sprintf, c(list(
    fmt = foo('Sensitivity: %.1f%% \033[32m=%d/%d\033[0m, 95%% exact CI (%.1f%%, %.1f%%)\n'), 
    1e2 * sens, x11, xr[2L]), as.list.default(1e2 * binom.test(x = x11, n = xr[2L])$conf.int))))
  cat(do.call(sprintf, c(list(
    fmt = foo('Specificity: %.1f%% \033[32m=%d/%d\033[0m, 95%% exact CI (%.1f%%, %.1f%%)\n'),
    1e2 * spec, x00, xr[1L]), as.list.default(1e2 * binom.test(x = x00, n = xr[1L])$conf.int))))
  
  if (!missing(prevalence)) {
    if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
        any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
    ppv_ <- ppv(prevalence, sensitivity = sens, specificity = spec)
    npv_ <- npv(prevalence, sensitivity = sens, specificity = spec)
    cat('\n')
    cat(sprintf(fmt = foo('Positive Predictive Value: %.3g%%, \033[36mprevalence %.3g%%\033[0m\n'), 1e2*ppv_, 1e2*prevalence), sep = '')
    cat('\n')
    cat(sprintf(fmt = foo('Negative Predictive Value: %.3g%%, \033[36mprevalence %.3g%%\033[0m\n'), 1e2*npv_, 1e2*prevalence), sep = '')
    cat('\n')
  } else {
    cat(do.call(sprintf, c(list(
      #fmt = 'Positive Predictive Value (unk. prevalence): %.1f%% (=%d/%d), 95%% exact CI (%.1f%%, %.1f%%)\n',
      fmt = foo('Positive Predictive Value: %.1f%% \033[32m=%d/%d\033[0m, 95%% exact CI (%.1f%%, %.1f%%)\n'),
      1e2 * x11/xc[2L], x11, xc[2L]), as.list.default(1e2 * binom.test(x = x11, n = xc[2L])$conf.int))))
    cat(do.call(sprintf, c(list(
      #fmt = 'Negative Predictive Value (unk. prevalence): %.1f%% (=%d/%d), 95%% exact CI (%.1f%%, %.1f%%)\n',
      fmt = foo('Negative Predictive Value: %.1f%% \033[32m=%d/%d\033[0m, 95%% exact CI (%.1f%%, %.1f%%)\n'),
      1e2 * x00/xc[1L], x00, xc[1L]), as.list.default(1e2 * binom.test(x = x00, n = xc[1L])$conf.int))))
    cat('\n')
  }
  
  if (FALSE) {
    #  chisq <- sum(x) * (x11*x00 - x[2L,1L]*x[1L,2L])^2 / prod(xr, xc)
    #  stopifnot(all.equal(chisq, unname(chisq.test(x, correct = FALSE)$statistic)))
    
    # relative risk
    #  risks <- x[1L,] / .colSums(x, m = 2L, n = 2L)
    #  logRR <- unname(log(risks[1L]) - log(risks[2L])) # Equation (12.7.2) (Page 644), Daniel Biostatistics, 10th
    #  logRR_sd <- logRR / sqrt(chisq)
    #  cat(do.call(sprintf, args = c(list(
    #    fmt = 'Relative Risk: %.2f (=(%d/%d)/(%d/%d)), 95%% CI (%.2f, %.2f), p = %.3f\n',
    #    exp(logRR), x11, xc[1L], x[1L,2L], xc[2L]
    #  ), 
    #  as.list.default(exp(logRR + qnorm(c(.025, .975)) * logRR_sd)),
    #  list(pnorm(abs(logRR), sd = logRR_sd, lower.tail = FALSE)))))
    
    # odds ratio
    #  odds <- x[1L,] / x[2L,]
    #  logOR <- unname(log(odds[1L]) - log(odds[2L])) # Equation (12.7.4) (Page 646), Daniel Biostatistics, 10th
    #  logOR_sd <- logOR / sqrt(chisq)
    #  cat(do.call(sprintf, args = c(list(
    #    fmt = 'Odds Ratio: %.2f (=(%d/%d)/(%d/%d)), 95%% CI (%.2f, %.2f), p = %.3f\n',
    #    exp(logOR), x11, x[2L,1L], x[1L,2L], x00
    #  ), 
    #  as.list.default(exp(logOR + qnorm(c(.025, .975)) * logOR_sd)),
    #  list(pnorm(abs(logOR), sd = logOR_sd, lower.tail = FALSE)))))
    
    #  cat('\n')
  } # have not flipped
  
  cat(do.call(sprintf, c(list(
    fmt = foo('Diagnose Accuracy: %.1f%% \033[32m=(%d+%d)/%d\033[0m, 95%% exact CI (%.1f%%, %.1f%%)\n\n'),
    1e2 * (x11+x00)/sum(x), x11, x00, sum(x)), as.list.default(1e2 * binom.test(x = x11+x00, n = sum(x))$conf.int))))
  
  # @importFrom e1071 classAgreement
  #kp <- e1071::classAgreement(x)$kappa # no confidence interval
  kp_ <- Kappa(x)
  kp_ci_ <- confint(kp_) # vcd:::confint.Kappa
  kp <- kp_$Weighted['value']
  kp_ci <- kp_ci_[rownames(kp_ci_) == 'Weighted',]
  cat(sprintf(
    fmt = foo('Cohen\'s Agreement \u03ba = %.2f, %s, 95%% exact CI (%.2f, %.2f)\n'), 
    kp, 
    sprintf(fmt = foo('\033[1;35m%s\033[0m'), as.character.factor(cut.default(
      # \url{https://en.wikipedia.org/wiki/Cohen\%27s_kappa}
      x = kp, breaks = c(-Inf, 0, .2, .4, .6, .8, 1), 
      labels = c('no', 'slight', 'fair', 'moderate', 'substantial', 'almost perfect'), 
      right = TRUE, include.lowest = TRUE
    ))), kp_ci[1L], kp_ci[2L]))

  return(invisible())
  
}




#' @title Create R Markdown Script for [binTab] Object
#' 
#' @description
#' Method dispatch to [binTab] for S3 generic `rmd_` (in a different master package).
#' 
#' @param x a [binTab]
#' 
#' @param xnm \link[base]{language} or \link[base]{character} scalar, call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [rmd_.binTab] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export rmd_.binTab
#' @export
rmd_.binTab <- function(x, xnm = substitute(x), ...) {
  if (is.language(xnm)) xnm <- deparse1(xnm)
  if (!is.character(xnm) || length(xnm) != 1L || is.na(xnm) || !nzchar(xnm)) stop('illegal `xnm`')
  
  # if (!identical(make.names(xnm), xnm)) stop('illegal `xnm`')
  # make.names('content[[1L]][[2L]]') # not what I want
  
  c(
    '```{r comment = NA}', 
    paste0('print(', xnm, ', ansi = FALSE)'), # invoke ?DanielBiostatistics10th::print.binTab
    '```'
  )
}








#' @title Predictive Values
#' 
#' @description
#' Positive and negative predictive values.
#' 
#' @param prevalence \link[base]{double} scalar or \link[base]{vector}
#' 
#' @param sensitivity,specificity \link[base]{double} scalars
#' 
#' @details
#' Function [ppv] calculates positive predictive values.
#' 
#' Function [npv] calculates negative predictive values.
#' 
#' @returns
#' Functions [ppv] and [npv] return \link[base]{double} scalar or \link[base]{vector}.
#' 
#' @name predval
#' @keywords internal
#' @export
ppv <- function(prevalence, sensitivity, specificity) {
  if (!is.double(sensitivity) || length(sensitivity) != 1L || is.na(sensitivity) || sensitivity < 0 || sensitivity > 1) stop('illegal sensitivity')
  if (!is.double(specificity) || length(specificity) != 1L || is.na(specificity) || specificity < 0 || specificity > 1) stop('illegal specificity')
  if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
      any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
  (sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))
}

#' @rdname predval
#' @export
npv <- function(prevalence, sensitivity, specificity) {
  if (!is.double(sensitivity) || length(sensitivity) != 1L || is.na(sensitivity) || sensitivity < 0 || sensitivity > 1) stop('illegal sensitivity')
  if (!is.double(specificity) || length(specificity) != 1L || is.na(specificity) || specificity < 0 || specificity > 1) stop('illegal specificity')
  if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
      any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
  (specificity * (1-prevalence)) / (specificity * (1-prevalence) + (1-sensitivity) * prevalence)
}


