

#' @title \eqn{z}-test from Aggregated Statistics
#' 
#' @description 
#' 
#' \eqn{z}-test from aggregated statistics.
#' 
#' @param obs \link[base]{numeric} \link[base]{vector}, observations
#' 
#' @param xbar \link[base]{numeric} scalar \eqn{\bar{x}} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(\bar{x}_1, \bar{x}_2)}. 
#' In the case of two-sample tests, this could also be a \link[base]{numeric} scalar \eqn{\bar{x}_1-\bar{x}_2}.
#' 
#' @param sd \link[base]{numeric} scalar \eqn{\sigma} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(\sigma_1, \sigma_2)},
#' population standard deviation(s)
#' 
#' @param n \link[base]{integer} scalar \eqn{n} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(n_1, n_2)},
#' sample size(s)
#' 
#' @param null.value (optional) \link[base]{numeric} scalar or \link[base]{length}-2 \link[base]{vector}.
#' Null value(s) of the population mean(s) 
#' (\eqn{\mu_0}, \eqn{(\mu_{10}, \mu_{20})}, or \eqn{\mu_{10}-\mu_{20}}). 
#' If missing, only the confidence intervals will be computed.
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param conf.level \link[base]{numeric} scalar \eqn{(1-\alpha)}, confidence level, default 0.95
#' 
#' @param ... potential arguments, currently not in use 
#' 
#' @details  
#' 
#' Function [z_test()] performs one- or two-sample \eqn{z}-test 
#' using the aggregated statistics of sample mean(s) and sample size(s) when 
#' `null.value` is provided.  Otherwise, only the confidence interval based on 
#' \eqn{z}-distribution is computed.
#' 
#' @return 
#' Function [z_test()] returns an `'htest'` object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' @keywords internal
#' @export
z_test <- function(
    xbar = mean(obs), 
    n = length(obs), 
    obs,
    sd, 
    null.value, 
    alternative = c('two.sided', 'less', 'greater'), 
    conf.level = .95, 
    ...
) {
  
  if (!is.numeric(xbar) || anyNA(xbar)) stop('Illegal sample mean(s)')
  if (!is.numeric(sd) || anyNA(sd) || any(sd <= 0)) stop('Illegal population standard deviation(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (has_null <- !missing(null.value)) {
    if (!is.numeric(null.value) || !any((n0 <- length(null.value)) == 1:2) || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 or len-2 number')
  }
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xbar = xbar, sd = sd, n = n) # vector recycling, let warn
  xbar <- tmp$xbar
  sd <- tmp$sd
  n <- tmp$n
  dname <- sprintf(fmt = '%.3g\u00B1%.3g, n=%d', xbar, sd, n) # console print, not figure
  
  if (length(xbar) == 1L) { # one sample z-test
    method <- 'One Sample z-test'
    std.err <- sd / sqrt(n)
    xbar0 <- xbar
    if (has_null && (n0 != 1L)) stop('`null.value` must be len-1 for one-sample z-test')
    
  } else if (length(xbar) == 2L) { # two sample z-test
    method <- 'Two Sample z-test'
    std.err <- sqrt(sd[1L]^2/n[1L] + sd[2L]^2/n[2L])
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample means
      dname <- sprintf(fmt = '\u0394x\u0304=%.1f (\u00B1%.3g vs. \u00B1%.3g)', xbar0, sd[1L], sd[2L])
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    if (has_null && (n0 == 2L)) null.value <- null.value[1L] - null.value[2L]
      
  } else stop('should not come here')
  
  if (has_null) zstat <- (xbar0 - null.value) / std.err
  switch(alternative, less = {
    if (has_null) pval <- pnorm(zstat, lower.tail = TRUE)
    cint0 <- c(-Inf, qnorm(conf.level, lower.tail = TRUE))
  }, greater = {
    if (has_null) pval <- pnorm(zstat, lower.tail = FALSE)
    cint0 <- c(qnorm(conf.level, lower.tail = FALSE), Inf)
  }, two.sided = {
    if (has_null) pval <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    cint0 <- c(-1, 1) * qnorm((1 - conf.level)/2, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * std.err
  attr(cint, which = 'conf.level') <- conf.level
  class(cint) <- c('conf.int', class(cint))
  if (!has_null) return(cint)
  
  ret <- list(
    statistic = setNames(zstat, nm = 'z'),
    p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(n), '1' = 'mean', '2' = 'mean-difference')), 
    stderr = std.err, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}






