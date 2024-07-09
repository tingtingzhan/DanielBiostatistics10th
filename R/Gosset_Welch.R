
# Do NOT edit DanielBiostatistics10th/R/Gosset_Welch.R  
# Edit tzh/R/Gosset_Welch.R

# https://en.wikipedia.org/wiki/Student%27s_t-test
# https://en.wikipedia.org/wiki/Welch%27s_t-test
# https://en.wikipedia.org/wiki/Welch–Satterthwaite_equation


#' @title Two-Sample Student's \eqn{t}-statistic and Welch–Satterthwaite Equation
#' 
#' @description
#' To determine the degree of freedom, as well as the standard error,
#' of two-sample \eqn{t}-statistic, with or without the equal-variance assumption.
#' 
#' @param s1,s0 (optional) \link[base]{double} scalars or \link[base]{vector}s, 
#' sample standard deviations \eqn{s_1} and \eqn{s_0} of the treatment and control sample, respectively
#' 
#' @param v1,v0 \link[base]{double} scalars or \link[base]{vector}s, 
#' sample variances of the treatment and control sample, respectively. 
#' Default \eqn{v_1=s_1^2}, \eqn{v_0=s_0^2}.
#' 
#' @param n1,n0 \link[base]{integer} scalars or \link[base]{vector}s, 
#' sample sizes of the treatment and control sample, respectively
#' 
#' @param var.equal \link[base]{logical} scalar, 
#' whether to treat the two variances \eqn{v_1} and \eqn{v_0} as being equal 
#' when calculating the degree of freedom and the standard error of the mean-difference.
#' If `FALSE` (default), Welch–Satterthwaite equation is used.
#' If `TRUE`, the original two-sample \eqn{t}-test from William Sealy Gosset is used.
#' See \link[stats]{t.test.default}.
#' 
#' @returns 
#' 
#' Function [Gosset_Welch] returns a \link[base]{numeric} scalar of the degree of freedom, 
#' with a \link[base]{numeric} scalar attribute `'stderr'` of the standard error of the mean-difference.
#' 
#' @references 
#' Student's \eqn{t}-test by William Sealy Gosset, \doi{10.1093/biomet/6.1.1}.
#' 
#' Welch–Satterthwaite equation by Bernard Lewis Welch and F. E. Satterthwaite, \doi{10.2307/3002019} and \doi{10.1093/biomet/34.1-2.28}.
#' 
# \url{https://en.wikipedia.org/wiki/Student%27s_t-test}
#' 
#' @seealso 
#' \link[stats]{t.test}
#' 
#' @examples 
#' x = rnorm(32L, sd = 1.6); y = rnorm(57L, sd = 2.1)
#' vx = var(x); vy = var(y); nx = length(x); ny = length(y)
#' t.test(x, y, var.equal = FALSE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v0 = vy, n1 = nx, n0 = ny, var.equal = FALSE)
#' t.test(x, y, var.equal = TRUE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v0 = vy, n1 = nx, n0 = ny, var.equal = TRUE)
#' 
#' @keywords internal
#' @export
Gosset_Welch <- function(s1, s0, v1 = s1^2, v0 = s0^2, n1, n0, var.equal = FALSE) {
  if (anyNA(v1) || anyNA(v0) || anyNA(n1) || anyNA(n0)) stop('do not allow missing')
  if (!is.logical(var.equal) || length(var.equal) != 1L || is.na(var.equal)) stop('`var.equal` must be len-1 logical')
  
  if (var.equal) {
    vp <- ((n1-1L)*v1+(n0-1L)*v0) / (n1+n0-2L) # 'pooled variance'
    vd <- vp * (1/n1 + 1/n0) # variance of sample-mean *d*ifference
    ret <- n1 + n0 - 2L
  } else {
    .v1 <- v1/n1
    .v0 <- v0/n0
    vd <- .v1 + .v0
    ret <- vd^2 / (.v1^2/(n1-1L) + .v0^2/(n0-1L))
  }
  attr(ret, which = 'stderr') <- sqrt(vd)
  return(ret)
}



