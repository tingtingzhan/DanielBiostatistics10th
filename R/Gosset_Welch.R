
# https://en.wikipedia.org/wiki/Student%27s_t-test
# https://en.wikipedia.org/wiki/Welch%27s_t-test
# https://en.wikipedia.org/wiki/Welch–Satterthwaite_equation


#' @title Degree of Freedom and Standard Error of Two-Sample \eqn{t}-statistic, by Student and Welch–Satterthwaite Equation
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
#' @param c1,c0 \link[base]{double} scalars or \link[base]{vector}s, 
#' multipliers \eqn{c_1} and \eqn{c_0} of the treatment and control sample, respectively.
#' Default \eqn{c_1=c_0=1}
#' 
#' @param n1,n0 \link[base]{integer} scalars or \link[base]{vector}s, 
#' sample sizes \eqn{n_1} and \eqn{n_0} of the treatment and control sample, respectively
#' 
#' @param var.equal \link[base]{logical} scalar, 
#' whether to assume \eqn{v_1=v_0}, default `FALSE`,
#' see \link[stats]{t.test.default}.
#' 
#' @details
#' If \eqn{v_1=v_0} is assumed, the two-sample \eqn{t}-statistic 
#' from William Sealy Gosset (a.k.a., Student) has 
#' standard error \eqn{s_{\bar\Delta}=s_p\sqrt{1/n_1+1/n_0}}
#' with degree-of-freedom \eqn{n_1+n_0-2},
#' where \eqn{s_p} is the pooled standard deviation,
#' \deqn{s^2_p=\dfrac{(n_1-1)v_1+(n_0-1)v_0}{n_1+n_0-2}}
#' 
#' If \eqn{v_1\neq v_0}, the two-sample \eqn{t}-statistic has 
#' standard error \eqn{s_{\bar\Delta}=\sqrt{v_1/n_1 + v_0/n_0}} with 
#' degree of freedom based on Welch–Satterthwaite equation,
#' \deqn{\dfrac{\left(\dfrac{v_1}{n_1}+\dfrac{v_0}{n_0}\right)^2}{\dfrac{(v_1/n_1)^2}{n_1-1}+\dfrac{(v_0/n_0)^2}{n_0-1}}}
#' 
#' @returns 
#' 
#' Function [Gosset_Welch] returns a \link[base]{numeric} scalar or \link[base]{vector} 
#' of the degree of freedom, with \link[base]{attributes},
#' \describe{
#' \item{`attr(., 'stderr')`}{\link[base]{numeric}, standard error \eqn{s_{\bar\Delta}}}
#' \item{`attr(., 'stderr2')`}{\link[base]{numeric}, \eqn{s^2_{\bar\Delta}}, variance of \eqn{\bar\Delta}}
#' } 
#' 
#' @references 
#' Student's \eqn{t}-test by William Sealy Gosset, \doi{10.1093/biomet/6.1.1}.
#' 
#' Welch–Satterthwaite equation by Bernard Lewis Welch and F. E. Satterthwaite, \doi{10.2307/3002019} and \doi{10.1093/biomet/34.1-2.28}.
#' 
# \url{https://en.wikipedia.org/wiki/Student%27s_t-test}
#' 
#' @examples 
#' x = rnorm(32L, sd = 1.6); y = rnorm(57L, sd = 2.1)
#' vx = var(x); vy = var(y); nx = length(x); ny = length(y)
#' t.test(x, y, var.equal = FALSE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v0 = vy, n1 = nx, n0 = ny, var.equal = FALSE)
#' t.test(x, y, var.equal = TRUE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v0 = vy, n1 = nx, n0 = ny, var.equal = TRUE)
#' @keywords internal
#' @export
Gosset_Welch <- function(
    s1, s0, 
    c1 = 1, c0 = 1,
    v1 = s1^2, v0 = s0^2, 
    n1, n0, 
    var.equal = FALSE
) {
  
  if (anyNA(v1) || anyNA(v0) || anyNA(n1) || anyNA(n0)) stop('do not allow missing')
  if (!is.logical(var.equal) || length(var.equal) != 1L || is.na(var.equal)) stop('`var.equal` must be len-1 logical')
  
  v1 <- c1^2 * v1
  v0 <- c0^2 * v0
  
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
  attr(ret, which = 'stderr2') <- vd
  return(ret)
}



