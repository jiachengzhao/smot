#' Add Linear Fitting Equation
#'
#' \code{addlmeq} produces an equation expression for a simple linear fitting to be added in a plot.
#' @param fit an object of class "lm" produced by simple linear models.
#' @param decimal displayed decimal places.
#'
#' @return An equation expression for the given simple linear fitting.
#' @export
#'
#' @seealso \code{\link{addr2}}.
#'
#' @examples
#' fit = lm(dist ~ speed, data = cars)
#' plot(cars)
#' abline(fit, col = 'red')
#' title(addlmeq(fit, 2), adj = 0.1, line = -1.8, col.main = 'blue', cex.main = 1.1)

addlmeq = function(fit, decimal = 2) {
  if (coef(fit)[1] > 0) {
    eq = substitute(
      italic('y') ~ '=' ~ a * italic('x') + b,
      list(
        a = sprintf(paste0('%.', decimal, 'f'), as.numeric(coef(fit))[2]),
        b = sprintf(paste0('%.', decimal, 'f'), as.numeric(coef(fit))[1])
      )
    )
  } else {
    eq = substitute(
      italic('y') ~ '=' ~ a * italic('x') - b,
      list(
        a = sprintf(paste0('%.', decimal, 'f'), as.numeric(coef(fit))[2]),
        b = sprintf(paste0('%.', decimal, 'f'), abs(as.numeric(coef(fit))[1]))
      )
    )
  }
  return(eq)
}
