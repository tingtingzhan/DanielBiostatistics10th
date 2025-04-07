

#' @title Chapter 5, 6 and 7
#' 
#' @description 
#' 
#' Functions for Chapter 5, \emph{Some Important Sampling Distributions},
#' Chapter 6, \emph{Estimation} and 
#' Chapter 7, \emph{Hypothesis Testing}.
#' 
#' @param x \link[base]{integer} scalar or \link[base]{length}-2 \link[base]{vector}, 
#' number of positive count(s) of binary (i.e., \link[base]{logical}) variable(s)
#' 
#' @param obs \link[base]{vector}, observations,
#' currently used only in one-sample \eqn{z}-test on proportion [prop_CLT]
#' 
#' @param phat \link[base]{numeric} scalar \eqn{\hat{p}} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(\hat{p}_1, \hat{p}_2)}. 
#' In the case of two-sample tests, this could also be a \link[base]{numeric} scalar \eqn{\hat{p}_1-\hat{p}_2}
#' 
#' @param n \link[base]{integer} scalar \eqn{n} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(n_1, n_2)},
#' sample size(s)
#' 
#' @param null.value (optional) \link[base]{numeric} scalar or \link[base]{length}-2 \link[base]{vector}.
#' Null value(s) of the population proportion(s) 
#' (\eqn{p_0}, \eqn{(p_{10}, p_{20})}, or \eqn{p_{10}-p_{20}})
#' for function [prop_CLT()].
#' If missing, only the confidence intervals will be computed.
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' either `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param conf.level \link[base]{numeric} scalar \eqn{(1-\alpha)}, confidence level, default 0.95
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @details  
#' Function [prop_CLT()] performs one- or two-sample \eqn{z}-test on proportion(s),
#' using Central Limit Theorem when `null.value` is provided.  
#' Otherwise, only the confidence interval based on \eqn{z}-distribution is computed.
#' 
#' @return 
#' Function [prop_CLT()] returns an \link[stats:prop.test]{htest} object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' @seealso \link[stats]{prop.test}
#' 
#' @keywords internal
#' @importFrom stats pnorm setNames
#' @export
prop_CLT <- function(x, n, obs, phat = x/n, null.value, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!missing(obs)) {
    if (!is.logical(obs) || !length(obs)) stop('Boolean observations illegal')
    # overwrite user provided `x`, `n` and `phat`
    obs <- obs[!is.na(obs)]
    x <- sum(obs) 
    n <- length(obs)
    phat <- x/n
  }
  
  if (!missing(x)) {
    if (!is.integer(x) || anyNA(x) || any(x < 0, x > n)) stop('Number of positive count(s) `x` must be non-negative integers')
  }  
  if (!is.numeric(phat) || anyNA(phat) || any(phat < 0, phat > 1)) stop('Sample proportion(s) `phat` must be in [0, 1]')
  if (!is.integer(n) || anyNA(n) || any(n <= 0L)) stop('Number of total count(s) `n` must be positive integers')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (has_null <- !missing(null.value)) {
    if (!is.numeric(null.value) || !any((n0 <- length(null.value)) == 1:2) || anyNA(null.value)) stop('Hypothesized proportion (difference) must be len-1 or len-2 number')
  }
  alternative <- match.arg(alternative)
  
  tmp <- if (missing(x)) {
    data.frame(phat = phat, n = n) # vector recycling, let warn
  } else data.frame(x = x, phat = phat, n = n)
  phat <- tmp[['phat']]
  n <- tmp$n
  dname <- sprintf(fmt = '%.1f%% (n=%d)', 1e2*phat, n)
  
  if (length(n) == 1L) { # one sample test
    method <- 'One Sample z-test on Proportion'
    if (has_null) {
      if (n0 != 1L) stop('`null.value` must be len-1 for one-sample z-test')
      null_val <- null.value
    }
    std.err <- if (has_null) sqrt(null.value * (1-null.value) / n) else sqrt(phat * (1-phat) / n)
    phat0 <- phat
    
  } else if (length(n) == 2L) { # two sample test
    method <- 'Two Sample z-test on Proportions'
    if (has_null) {
      p.equal <- FALSE
      if (n0 == 1L) {
        null_val <- null.value
        if (null.value == 0) {
          p.equal <- TRUE
        } else stop('must specify the two population proportions')
      } else { # n0 == 2L
        null_val <- null.value[1L] - null.value[2L]
        if (isTRUE(all.equal(null_val, 0))) p.equal <- TRUE
      }
    }
    std.err <- if (!has_null) {
      sqrt(sum(phat * (1-phat) / n))
    } else if (p.equal) {
      phat_eq <- sum(phat * n) / sum(n)
      sqrt(phat_eq * (1-phat_eq) * sum(1/n))
    } else sqrt(sum(null.value * (1-null.value) / n))
    if (isTRUE(all.equal.numeric(phat[1L], phat[2L]))) {
      phat0 <- phat[1L] # input is actually (phat1 - phat2), difference of sample proportions
      dname <- sprintf(fmt = '\u0394p\u0302=%.1f%%', 1e2*phat0)
    } else {
      phat0 <- phat[1L] - phat[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    
  } else stop('should not come here')
  
  if (has_null) zstat <- (phat0 - null_val) / std.err
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
  
  cint <- phat0 + cint0 * std.err
  attr(cint, which = 'conf.level') <- conf.level
  if (!has_null) return(cint)
  
  ret <- list(
    statistic = setNames(zstat, nm = 'z'),
    p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null_val, nm = switch(length(n), '1' = 'proportion', '2' = 'proportion-difference')),
    stderr = std.err, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}













