#' Add R-Square
#'
#' \code{addr2} produces an r-squared (r2) expression (r2 = ...) given a vector of observation and prediction.
#' @param obs vector of observation.
#' @param pred vector of prediction.
#' @param decimal displayed decimal places.
#'
#' @return An r2 expression for the given vectors of observation and prediction.
#' @export
#'
#' @examples
#' fit = lm(dist ~ speed, data = cars)
#' plot(cars)
#' abline(fit, col = 'red')
#' title(addlmeq(fit, 2), adj = 0.1, line = -1.8, col.main = 'blue', cex.main = 1.1)
#' title(addr2(cars$dist, predict(fit)), adj = 0.08, line = -2.8, col.main = 'blue', cex.main = 1.1)

addr2 = function(obs, pred, decimal = 2) {
  r2 = substitute(
    italic('r') ^ 2 ~ '=' ~ r2,
    list(
      r2 = sprintf(paste0('%.', decimal, 'f'), bias(obs, pred)[1])
    )
  )
  return(r2)
}
