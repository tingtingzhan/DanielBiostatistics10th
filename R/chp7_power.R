

#' @title Chapter 7: Power Curve
#' 
#' @description 
#' 
#' Functions for Chapter 7, \emph{Hypothesis Testing}.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}, mean parameter(s) \eqn{\mu_1} in the alternative hypothesis
#' 
#' @param null.value \link[base]{numeric} scalar, mean parameter \eqn{\mu_0} in the null hypothesis
#' 
#' @param sd \link[base]{numeric} scalar, population standard deviation \eqn{\sigma}
#' 
#' @param n \link[base]{integer} scalar, sample size \eqn{n}
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis, 
#' either `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param sig.level \link[base]{numeric} scalar, significance level (i.e., Type-I-error rate), default \eqn{.05}
#' 
#' @details 
#' Function [power_z] calculates the powers at each element of the alternative parameters \eqn{\mu_1}, for one-sample \eqn{z}-test
#' \itemize{
#' \item{\eqn{H_0: \mu = \mu_0} vs. \eqn{H_A: \mu \neq \mu_0}, if `alternative = 'two.sided'`}
#' \item{\eqn{H_0: \mu \leq \mu_0} vs. \eqn{H_A: \mu > \mu_0}, if `alternative = 'greater'`}
#' \item{\eqn{H_0: \mu \geq \mu_0} vs. \eqn{H_A: \mu < \mu_0}, if `alternative = 'less'`}
#' }
#' 
#' @return 
#' Function [power_z] returns a `'power_z'` object, 
#' which inherits from `'power.htest'` class. 
#' 
#' @seealso \link[stats]{power.t.test}
#' 
#' @example inst/extexample/Chapter7_power.R 
#' 
#' @name Chapter07_power
#' @importFrom stats pnorm
#' @export
power_z <- function(
    x, 
    null.value, 
    sd, n, 
    alternative = c('two.sided', 'less', 'greater'), 
    sig.level = .05
) {
  
  alternative <- match.arg(alternative)
  
  std.err <- sd / sqrt(n)
  
  powerFun <- function(x, alternative) {
    
    switch(alternative, two.sided = {
      rr <- qnorm(p = c(sig.level/2, 1-sig.level/2), mean = null.value, sd = std.err) # 2-sided rejection region
      ret <- pnorm(q = rr[1L], mean = x, sd = std.err, lower.tail = TRUE) + # P(White), in left-RR
        pnorm(q = rr[2L], mean = x, sd = std.err, lower.tail = FALSE) # P(White), in Right-RR  
      attr(ret, which = 'reject') <- sprintf(fmt = '<%.3f or >%.3f', rr[1L], rr[2L])
      
    }, 'less' = {
      rr <- qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = TRUE) # P(X_bar < rr) = sig.level
      ret <- pnorm(q = rr, mean = x, sd = std.err, lower.tail = TRUE) # one-sided P(White)
      attr(ret, which = 'reject') <- sprintf(fmt = '<%.3f', rr)
      
    }, 'greater' = {
      rr <- qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = FALSE) # P(X_bar > rr) = sig.level
      ret <- pnorm(q = unclass(rr), mean = x, sd = std.err, lower.tail = FALSE) # one-sided P(White)
      attr(ret, which = 'reject') <- sprintf(fmt = '>%.3f', rr)
    })
    
    return(ret)
    
  }
  
  power <- powerFun(x, alternative = alternative)
  
  ret <- list(
    mu = x, 
    null.value = null.value, 
    sd = sd, n = n,
    #std.err = std.err,
    power = power,
    sig.level = sig.level,
    reject = attr(power, which = 'reject', exact = TRUE),
    alternative = alternative, 
    method = 'One-Sample z-Test'
  )
  attr(ret, which = 'power.function') <- powerFun
  class(ret) <- c('power_z', 'power.htest')
  return(ret)

}


