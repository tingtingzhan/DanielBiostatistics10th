

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
#' @param xbar \link[base]{numeric} scalar or 
#' \link[base]{length}-2 \link[base]{vector}. 
#' Sample mean(s) for \link[base]{numeric} variable(s) \eqn{\bar{x}} or \eqn{(\bar{x}_1, \bar{x}_2)}.
#' Sample proportion(s) for binary (i.e., \link[base]{logical}) variable(s) \eqn{\hat{p}} or \eqn{(\hat{p}_1, \hat{p}_2)}. 
#' In the case of two-sample tests, this could also be a \link[base]{numeric} scalar indicating the difference in 
#' sample means \eqn{\bar{x}_1-\bar{x}_2} or sample proportions \eqn{\hat{p}_1-\hat{p}_2}
#' 
# @param xsd \link[base]{numeric} scalar \eqn{\sigma_{\bar{x}}} or 
# \link[base]{length}-2 \link[base]{vector} \eqn{(\sigma_{\bar{x}_1}, \sigma_{\bar{x}_2})},
# sample standard deviation(s)
# wrong!!!!
#' @param xsd \link[base]{numeric} scalar \eqn{s} or 
#' \link[base]{length}-2 \link[base]{vector} \eqn{(s_1, s_2)},
#' sample standard deviation(s)
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
#' (\eqn{\mu_0}, \eqn{(\mu_{10}, \mu_{20})}, or \eqn{\mu_{10}-\mu_{20}}) 
#' for functions [aggregated_z] and [aggregated_t].
#' Null value(s) of the population proportion(s) 
#' (\eqn{p_0}, \eqn{(p_{10}, p_{20})}, or \eqn{p_{10}-p_{20}})
#' for [prop_CLT].
#' Null value(s) of the population variance(s) (ratio)
#' (\eqn{\sigma^2_0}, \eqn{(\sigma^2_{10}, \sigma^2_{20})}, or \eqn{\sigma^2_{10}/\sigma^2_{20}})
#' for function [aggregated_var].
#' If missing, only the confidence intervals will be computed.
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' either `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param conf.level \link[base]{numeric} scalar \eqn{(1-\alpha)}, confidence level, default 0.95
#' 
#' @param var.equal \link[base]{logical} scalar, whether to treat the two population variances as being equal 
#' (default `FALSE`) in function [aggregated_t]
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @details  
#' 
#' Function [aggregated_z] performs one- or two-sample \eqn{z}-test 
#' using the aggregated statistics of sample mean(s) and sample size(s) when 
#' `null.value` is provided.  Otherwise, only the confidence interval based on 
#' \eqn{z}-distribution is computed.
#' 
#' Function [aggregated_t] performs one- or two-sample \eqn{t}-test 
#' using the aggregated statistics of sample mean(s), sample standard deviation(s) and sample size(s)
#' when `null.value` is provided.  Otherwise, only the confidence interval based on 
#' \eqn{t}-distribution is computed.
#' 
#' Function [prop_CLT] performs one- or two-sample \eqn{z}-test on proportion(s),
#' using Central Limit Theorem when `null.value` is provided.  
#' Otherwise, only the confidence interval based on \eqn{z}-distribution is computed.
#' 
#' Function [aggregated_var] performs one-sample \eqn{\chi^2}-test on variance, 
#' or two-sample \eqn{F}-test on variances, using the aggregated statistics of 
#' sample standard deviation(s) and sample size(s) when `null.value` is provided.  
#' Otherwise, only the confidence interval based on \eqn{\chi^2}- or \eqn{F}-distribution is computed.
#' 
#' @return 
#' Function [aggregated_z] returns an `'htest'` object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' Function [aggregated_t] returns an \link[stats:t.test]{htest} object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' Function [prop_CLT] returns an \link[stats:prop.test]{htest} object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' Function [aggregated_var] returns an \link[stats:var.test]{htest} object when `null.value` is provided, 
#' otherwise returns a length-two \link[base]{numeric} vector.
#' 
#' @seealso \link[stats]{t.test} \link[stats]{prop.test} \link[stats]{var.test}
#' 
#' @example inst/extexample/Chapter5.6.7.R 
#' 
#' @name Chapter05to07
#' @importFrom stats pnorm setNames
#' @export
aggregated_z <- function(xbar, n, sd, null.value, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!is.numeric(xbar) || anyNA(xbar)) stop('Illegal sample mean(s)')
  if (!is.numeric(sd) || anyNA(sd) || any(sd <= 0)) stop('Illegal population standard deviation(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (do_test <- !missing(null.value)) {
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
    if (do_test && (n0 != 1L)) stop('`null.value` must be len-1 for one-sample z-test')
    
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
    if (do_test && (n0 == 2L)) null.value <- null.value[1L] - null.value[2L]
      
  } else stop('should not come here')
  
  if (do_test) zstat <- (xbar0 - null.value) / std.err
  switch(alternative, less = {
    if (do_test) pval <- pnorm(zstat, lower.tail = TRUE)
    cint0 <- c(-Inf, qnorm(conf.level, lower.tail = TRUE))
  }, greater = {
    if (do_test) pval <- pnorm(zstat, lower.tail = FALSE)
    cint0 <- c(qnorm(conf.level, lower.tail = FALSE), Inf)
  }, two.sided = {
    if (do_test) pval <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    cint0 <- c(-1, 1) * qnorm((1 - conf.level)/2, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * std.err
  attr(cint, which = 'conf.level') <- conf.level
  if (!do_test) return(cint)
  
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



#' @rdname Chapter05to07
#' @importFrom stats pt qt setNames
#' @export
aggregated_t <- function(xbar, xsd, n, null.value, var.equal = FALSE, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!is.numeric(xbar) || anyNA(xbar)) stop('Illegal sample mean(s)')
  if (!is.numeric(xsd) || anyNA(xsd) || any(xsd <= 0)) stop('Illegal sample standard deviation(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (do_test <- !missing(null.value)) {
    if (!is.numeric(null.value) || !any((n0 <- length(null.value)) == 1:2) || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 or len-2 number')
  }
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xbar = xbar, xsd = xsd, n = n) # vector recycling, let warn
  xbar <- tmp$xbar
  xsd <- tmp$xsd
  n <- tmp$n
  dname <- sprintf(fmt = '%.3g\u00B1%.3g', xbar, xsd)
  
  if (length(xbar) == 1L) { # one sample t-test
    method <- 'One Sample t-test'
    df <- n - 1L
    std.err <- xsd / sqrt(n)
    xbar0 <- xbar
    if (do_test && (n0 != 1L)) stop('`null.value` must be len-1 for one-sample z-test')
    
  } else if (length(xbar) == 2L) { # two sample t-test
    method <- if (var.equal) 'Two Sample t-test (Equal-Variance)' else 'Welch Two Sample t-test'
    df <- Gosset_Welch(s1 = xsd[1L], s0 = xsd[2L], n1 = n[1L], n0 = n[2L], var.equal = var.equal)
    std.err <- attr(df, which = 'stderr', exact = TRUE)
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample means
      dname <- sprintf(fmt = '\u0394x\u0304=%.1f (\u00B1%.3g vs. \u00B1%.3g)', xbar0, xsd[1L], xsd[2L])
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    if (do_test && (n0 == 2L)) null.value <- null.value[1L] - null.value[2L]
    
  } else stop('should not come here')
  
  if (do_test) tstat <- (xbar0 - null.value) / std.err
  switch(alternative, less = {
    if (do_test) pval <- pt(tstat, df = df, lower.tail = TRUE)
    cint0 <- c(-Inf, qt(conf.level, df = df, lower.tail = TRUE))
  }, greater = {
    if (do_test) pval <- pt(tstat, df = df, lower.tail = FALSE)
    cint0 <- c(qt(conf.level, df = df, lower.tail = FALSE), Inf)
  }, two.sided = {
    if (do_test) pval <- 2 * pt(abs(tstat), df = df, lower.tail = FALSE)
    cint0 <- c(-1, 1) * qt((1 - conf.level)/2, df = df, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * std.err
  attr(cint, which = 'conf.level') <- conf.level
  if (!do_test) return(cint)
  
  ret <- list(
    statistic = setNames(tstat, nm = 't'), parameter = setNames(df, nm = 'df'), p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(xbar), '1' = 'mean', '2' = 'mean-difference')),
    stderr = std.err, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}



#' @rdname Chapter05to07
#' @importFrom stats pnorm setNames
#' @export
prop_CLT <- function(x, n, obs, xbar = x/n, null.value, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!missing(obs)) {
    if (!is.logical(obs) || !length(obs)) stop('Boolean observations illegal')
    # overwrite user provided `x`, `n` and `xbar`
    obs <- obs[!is.na(obs)]
    x <- sum(obs) 
    n <- length(obs)
    xbar <- x/n
  }
  
  if (!missing(x)) {
    if (!is.integer(x) || anyNA(x) || any(x < 0, x > n)) stop('Number of positive count(s) `x` must be non-negative integers')
  }  
  if (!is.numeric(xbar) || anyNA(xbar) || any(xbar < 0, xbar > 1)) stop('Sample proportion(s) `xbar` must be in [0, 1]')
  if (!is.integer(n) || anyNA(n) || any(n <= 0L)) stop('Number of total count(s) `n` must be positive integers')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (do_test <- !missing(null.value)) {
    if (!is.numeric(null.value) || !any((n0 <- length(null.value)) == 1:2) || anyNA(null.value)) stop('Hypothesized proportion (difference) must be len-1 or len-2 number')
  }
  alternative <- match.arg(alternative)
  
  tmp <- if (missing(x)) {
    data.frame(xbar = xbar, n = n) # vector recycling, let warn
  } else data.frame(x = x, xbar = xbar, n = n)
  xbar <- tmp[['xbar']]
  n <- tmp$n
  dname <- sprintf(fmt = '%.1f%% (n=%d)', 1e2*xbar, n)
  
  if (length(n) == 1L) { # one sample test
    method <- 'One Sample z-test on Proportion'
    if (do_test) {
      if (n0 != 1L) stop('`null.value` must be len-1 for one-sample z-test')
      null_val <- null.value
    }
    std.err <- if (do_test) sqrt(null.value * (1-null.value) / n) else sqrt(xbar * (1-xbar) / n)
    xbar0 <- xbar
    
  } else if (length(n) == 2L) { # two sample test
    method <- 'Two Sample z-test on Proportions'
    if (do_test) {
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
    std.err <- if (!do_test) {
      sqrt(sum(xbar * (1-xbar) / n))
    } else if (p.equal) {
      xbar_eq <- sum(xbar * n) / sum(n)
      sqrt(xbar_eq * (1-xbar_eq) * sum(1/n))
    } else sqrt(sum(null.value * (1-null.value) / n))
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample proportions
      dname <- sprintf(fmt = '\u0394p\u0302=%.1f%%', 1e2*xbar0)
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    
  } else stop('should not come here')
  
  if (do_test) zstat <- (xbar0 - null_val) / std.err
  switch(alternative, less = {
    if (do_test) pval <- pnorm(zstat, lower.tail = TRUE)
    cint0 <- c(-Inf, qnorm(conf.level, lower.tail = TRUE))
  }, greater = {
    if (do_test) pval <- pnorm(zstat, lower.tail = FALSE)
    cint0 <- c(qnorm(conf.level, lower.tail = FALSE), Inf)
  }, two.sided = {
    if (do_test) pval <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    cint0 <- c(-1, 1) * qnorm((1 - conf.level)/2, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * std.err
  attr(cint, which = 'conf.level') <- conf.level
  if (!do_test) return(cint)
  
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



#' @rdname Chapter05to07
#' @importFrom stats pchisq pf qchisq qf setNames
#' @export
aggregated_var <- function(xsd, n, null.value, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!is.numeric(xsd) || anyNA(xsd) || any(xsd <= 0)) stop('Illegal sample variances(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  if (do_test <- !missing(null.value)) {
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
    if (do_test) {
      if (n0 != 1L) stop('`null.value` must be len-1 for ', method)
      null.value <- setNames(null.value, nm = 'variance')
      v_stat <- setNames((df * xvar) / null.value, nm = 'Chi-Squared')
      pval <- unname(pchisq(v_stat, df = df))
    }
    switch(alternative, two.sided = {
      if (do_test) pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- df * xvar / qchisq(c(1-a2, a2), df = df)
    }, less = {
      cint <- c(0, df * xvar / qchisq(1 - conf.level, df = df))
    }, greater = {
      if (do_test) pval <- 1 - pval
      cint <- c(df * xvar / qchisq(conf.level, df = df), Inf)
    })
    
  } else if (length(xvar) == 2L) {
    method <- 'F test to compare two variances'
    dname <- paste(dname, collapse = ' vs. ')
    estimate <- setNames(xvar[1L] / xvar[2L], nm = 'Estimated Variances-Ratio')
    df <- setNames(df, nm = c('num df', 'denom df'))
    if (do_test) {
      if (n0 == 2L) null.value <- null.value[1L] / null.value[2L]
      null.value <- setNames(null.value, nm = 'variances-ratio')
      v_stat <- setNames(estimate / null.value, nm = 'F')
      pval <- pf(v_stat, df1 = df[1L], df2 = df[2L])
    }
    switch(alternative, two.sided = {
      if (do_test) pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- estimate / qf(c(1-a2, a2), df1 = df[1L], df2 = df[2L])
    }, less = {
      cint <- c(0, estimate/qf(1 - conf.level, df1 = df[1L], df2 = df[2L]))
    }, greater = {
      if (do_test) pval <- 1 - pval
      cint <- c(estimate/qf(conf.level, df1 = df[1L], df2 = df[2L]), Inf)
    })
    
  } else stop('should not come here')
  
  cint <- unname(cint)
  attr(cint, which = 'conf.level') <- conf.level
  if (!do_test) return(cint)
  
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












