

#' @title One-Sample \eqn{\chi^2}-test,
#' or Two-Sample \eqn{F}-test, on Variances
#' 
#' @description 
#' One-sample \eqn{\chi^2}-test,
#' or two-sample \eqn{F}-test, on variances.
#' 
#' @param obs \link[base]{numeric} \link[base]{vector}, observations
#' 
#' @param xsd \link[base]{numeric} scalar \eqn{s} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(s_1, s_2)},
#' sample standard deviation(s)
#' 
#' @param n \link[base]{integer} scalar \eqn{n} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(n_1, n_2)},
#' sample size(s)
#' 
#' @param null.value (optional) \link[base]{numeric} scalar or \link[base]{length}-2 \link[base]{vector}.
#' Null value(s) of the population variance(s) (ratio)
#' (\eqn{\sigma^2_0}, \eqn{(\sigma^2_{10}, \sigma^2_{20})}, or \eqn{\sigma^2_{10}/\sigma^2_{20}}).
#' If missing, only the confidence intervals will be computed.
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' either `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param conf.level \link[base]{numeric} scalar \eqn{(1-\alpha)}, confidence level, default 0.95
#' 
#' @param ... potential arguments, currently not in use 
#' 
#' @details  
#' Function [var_test()] performs one-sample \eqn{\chi^2}-test on variance, 
#' or two-sample \eqn{F}-test on variances, using the aggregated statistics of 
#' sample standard deviation(s) and sample size(s) when `null.value` is provided.  
#' Otherwise, only the confidence interval based on \eqn{\chi^2}- or \eqn{F}-distribution is computed.
#' 
#' @return 
#' Function [var_test()] returns an \link[stats:var.test]{htest} object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' @keywords internal
#' @export
var_test <- function(
    xsd = sd(obs), 
    n = length(obs), 
    obs,
    null.value, 
    alternative = c('two.sided', 'less', 'greater'), 
    conf.level = .95, 
    ...
) {
  
  if (!is.numeric(xsd) || anyNA(xsd) || any(xsd <= 0)) stop('Illegal sample variances(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (has_null <- !missing(null.value)) {
    if (!is.numeric(null.value) || !any((n0 <- length(null.value)) == 1:2) || anyNA(null.value)) stop('Hypothesized variance (ratio) must be len-1 or len-2 number')
  }
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xvar = xsd^2, n = n)
  xvar <- tmp[['xvar']]
  n <- tmp[['n']]
  df <- n - 1L
  dname <- sprintf(fmt = '\u03c32=%.2f (n=%d)', xvar, n)
  
  if (length(xvar) == 1L) {
    # essentially ?EnvStats::varTest
    estimate <- NULL # actually `xvar`, no need to output
    method <- 'Chi-squared test on one-sample variance'
    df <- setNames(df, nm = 'df')
    if (has_null) {
      if (n0 != 1L) stop('`null.value` must be len-1 for ', method)
      null.value <- setNames(null.value, nm = 'variance')
      v_stat <- setNames((df * xvar) / null.value, nm = 'Chi-Squared')
      pval <- pchisq(v_stat, df = df) |> unname()
    }
    switch(alternative, two.sided = {
      if (has_null) pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- df * xvar / qchisq(c(1-a2, a2), df = df)
    }, less = {
      cint <- c(0, df * xvar / qchisq(1 - conf.level, df = df))
    }, greater = {
      if (has_null) pval <- 1 - pval
      cint <- c(df * xvar / qchisq(conf.level, df = df), Inf)
    })
    
  } else if (length(xvar) == 2L) {
    method <- 'F test to compare two variances'
    dname <- paste(dname, collapse = ' vs. ')
    estimate <- setNames(xvar[1L] / xvar[2L], nm = 'Estimated Variances-Ratio')
    df <- setNames(df, nm = c('num df', 'denom df'))
    if (has_null) {
      if (n0 == 2L) null.value <- null.value[1L] / null.value[2L]
      null.value <- setNames(null.value, nm = 'variances-ratio')
      v_stat <- setNames(estimate / null.value, nm = 'F')
      pval <- pf(v_stat, df1 = df[1L], df2 = df[2L])
    }
    switch(alternative, two.sided = {
      if (has_null) pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- estimate / qf(c(1-a2, a2), df1 = df[1L], df2 = df[2L])
    }, less = {
      cint <- c(0, estimate/qf(1 - conf.level, df1 = df[1L], df2 = df[2L]))
    }, greater = {
      if (has_null) pval <- 1 - pval
      cint <- c(estimate/qf(conf.level, df1 = df[1L], df2 = df[2L]), Inf)
    })
    
  } else stop('should not come here')
  
  cint <- unname(cint)
  attr(cint, which = 'conf.level') <- conf.level
  class(cint) <- c('conf.int', class(cint))
  if (!has_null) return(cint)
  
  ret <- list(
    statistic = v_stat, parameter = df,
    p.value = pval, conf.int = cint, 
    estimate = estimate, null.value = null.value,
    alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}












