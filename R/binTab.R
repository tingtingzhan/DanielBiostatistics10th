
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
#' @param nm \link[base]{length}-2 \link[base]{character} \link[base]{vector}
#' 
#' @details
#' 
#' Function [binTab()] creates a \eqn{2\times 2} test-&-disease contingency table with layout
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
#' Function [binTab()] returns a two-by-two \link[base]{integer} \link[base]{matrix}.
#' 
#' @note 
#' Function `caret::confusionMatrix` does not provide confidence intervals of 
#' sensitivity, specificity, etc.
#' 
#' @examples 
#' matrix(c(7L, 3L, 8L, 6L), nrow = 2L) |> binTab()
#' matrix(c(7L, 3L, 8L, 6L), nrow = 2L, dimnames = list(X = c('a','b'), NULL)) |> 
#'  binTab()
#' @keywords internal
#' @export
binTab <- function(x, nm = c('Endpoint', 'Test or Risk')) {
  
  if (!is.matrix(x) || (typeof(x) != 'integer') || any(dim(x) != 2L)) stop('input must be 2*2 integer matrix')
  
  dnm <- dimnames(x)
  
  if (!length(dnm)) {
    
    dimnames(x) <- list(c('(-)', '(+)'), c('(-)', '(+)')) |> setNames(nm = nm)
    
  } else {

    ndnm <- names(dnm)
    if (!length(ndnm)) {
      names(dimnames(x)) <- nm
    } else if (!all(nzchar(ndnm))) {
      id <- !nzchar(ndnm)
      names(dimnames(x))[id] <- nm[id]
    } # else do nothing
    
    dimnames(x)[] <- dnm |> lapply(FUN = \(i) {
      if (!length(i)) return(c('(-)', '(+)'))
      if (!all(nzchar(i))) stop('do not allow zchar in rownames or colnames')
      if (i[1L] %notin% c('FALSE', '(-)')) i[1L] <- paste(i[1L], '(-)')
      if (i[2L] %notin% c('TRUE', '(+)')) i[2L] <- paste(i[2L], '(+)')
      return(i)
    })
    
  }
  
  class(x) <- c('binTab', 'table') # to invoke ?flextable:::as_flextable.table
  return(x)
  
}










#' @title Print Boolean Test-&-Disease and/or Risk-&-Disease Table 
#' 
#' @description 
#' Print Boolean test-&-disease and/or risk-&-disease table.
#' 
#' @param object a [binTab] object
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar or \link[base]{vector}, prevalence of disease
#' 
#' @param ... potential parameters, currently not in use 
#' 
#' @details
#' Function [summary.binTab] prints the diagnostic test characteristics, 
#' e.g., sensitivity, specificity, predictive values, and diagnostic accuracy,
#' together with their \eqn{95\%} Clopper-Pearson exact confidence intervals.
#' 
#' @returns 
#' Function [summary.binTab] \link[base]{invisible}-y returns a \link[base]{character} \link[base]{vector}.
#' 
#' @note
#' Function \link[e1071]{classAgreement} does not provide confidence interval of \eqn{\kappa}.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Diagnostic_odds_ratio}
#' 
#' @examples 
#' (x = array(c(95L, 10L, 31L, 82L), dim = c(2L, 2L)))
#' binTab(x)
#' summary(binTab(x))
#' print(binTab(x), prevalence = c(.0001, .001, .01))
#' @keywords internal
#' @importFrom flextable flextable autofit align
#' @importFrom scales label_percent
#' @export summary.binTab
#' @export
summary.binTab <- function(
    object, 
    prevalence, 
    ...
) {
  
  x <- object; object <- NULL
  
  x11 <- x[2L,2L] # (+,+)
  x00 <- x[1L,1L] # (-,-)
  xr <- .rowSums(x, m = 2L, n = 2L) # Disease (-) and (+)
  xc <- .colSums(x, m = 2L, n = 2L) # Test (-) and (+)
  
  sens <- x11 / xr[2L]
  spec <- x00 / xr[1L]
  
  ret <- data.frame(
    ' ' = c('Sensitivity', 'Specificity'),
    Estimated = c(sens, spec) |>
      label_percent(accuracy = .1)(),
    From = sprintf(fmt = '%d/%d', c(x11, x00), c(xr[2L], xr[1L])),
    '95% Exact CI' = c(
      binom.test(x = x11, n = xr[2L])$conf.int |>
        label_percent(accuracy = .1)() |>
        paste(collapse = ', '),
      binom.test(x = x00, n = xr[1L])$conf.int |>
        label_percent(accuracy = .1)() |>
        paste(collapse = ', ')
    ),
    check.names = FALSE
  )
  
  if (!missing(prevalence)) {
    
    if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
        any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
    ppv_ <- ppv(prevalence, sensitivity = sens, specificity = spec)
    npv_ <- npv(prevalence, sensitivity = sens, specificity = spec)
    
    ret <- ret |> 
      rbind(. = _, data.frame(
        ' ' = c('Positive Predictive Value', 'Negative Predictive Value'),
        Estimated = c(ppv_, npv_) |>
          label_percent(accuracy = .1)(),
        From = prevalence |> 
          label_percent(accuracy = .1, suffix = '% Prevalence')(),
        '95% Exact CI' = NA_character_,
        check.names = FALSE 
      ))

  } else {
    
    ret <- ret |> 
      rbind(. = _, data.frame(
        ' ' = c('Positive Predictive Value', 'Negative Predictive Value', 'Diagnose Accuracy'),
        Estimated = c(x11/xc[2L], x00/xc[1L], (x11+x00)/sum(x)) |>
          label_percent(accuracy = .1)(),
        From = c(
          sprintf(fmt = '%d/%d', c(x11, x00), c(xc[2L], xc[1L])),
          sprintf(fmt = '(%d+%d)/%d', x11, x00, sum(x))
        ),
        '95% Exact CI' = c(
          binom.test(x = x11, n = xc[2L])$conf.int |>
            label_percent(accuracy = .1)() |>
            paste(collapse = ', '),
          binom.test(x = x00, n = xc[1L])$conf.int |>
            label_percent(accuracy = .1)() |>
            paste(collapse = ', '),
          binom.test(x = x11+x00, n = sum(x))$conf.int |>
            label_percent(accuracy = .1)() |>
            paste(collapse = ', ')
        ),
        check.names = FALSE
      ))

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
  
  ret |>
    flextable() |>
    autofit(part = 'all') |>
    align(j = 1L, align = c('center'), part = 'all') |>
    align(j = ret |> seq_along() |> setdiff(y = 1L), align = c('right'), part = 'all')
  
}




#' @title [as_flextable.binTab()]
#' 
#' @param x `binTab`
#' 
#' @param ... ..
#' 
#' @importFrom patchwork plot_layout
#' @importFrom flextable as_flextable wrap_flextable
#' @export as_flextable.binTab
#' @export
as_flextable.binTab <- function(x, ...) {
  t1 <- NextMethod()
  t2 <- x |>
    summary.binTab()
  list(t1, t2) |>
    lapply(FUN = wrap_flextable) |>
    Reduce(f = `+`) +
    plot_layout(ncol = 1L)
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
#' Function [ppv()] calculates positive predictive values.
#' 
#' Function [npv()] calculates negative predictive values.
#' 
#' @returns
#' Functions [ppv()] and [npv()] return \link[base]{double} scalar or \link[base]{vector}.
#' 
#' @keywords internal
#' @name predval
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







