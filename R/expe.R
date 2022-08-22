#' Expectation for a Probability Density Function (PDF)
#'
#' \code{expe} gives the expected value for a pdf.
#'
#' @param data a random variable in form of vector.
#'
#' @examples
#' d = rpois(n = 1000, lambda = 10)
#' hist(d, prob = TRUE)
#' lines(density(d, adjust = 1.5), col = 'black', lwd = 2)
#' expe(d)
#' expe(d)$value
#'
#' @export

expe = function(data) {
  af = approxfun(density(data))
  integrand = function(x) {
    f1 = x
    f2 = af(x)
    f2[is.na(f2)] = 0
    return(f1 * f2)
  }
  return(
    integrate(integrand, -Inf, Inf)
  )
}
