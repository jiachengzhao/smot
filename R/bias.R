#' Bias
#'
#' \code{bias} computes r-square, bias and root mean square error (RMSE) given a vector of observation and prediction.
#' @param obs vector of observation.
#' @param pred vector of prediction.
#'
#' @return A data.frame containing r-square, bias and RMSE.
#' @export
#'
#' @examples
#' bias(1:10, 1:10 + rnorm(10))

bias = function(obs, pred) {
  n = length(obs) # sample size
  ssr = sum((obs - pred) ^ 2) # sum of squared residuals
  r2 = 1 - ssr / sum((obs - mean(obs)) ^ 2) # r2
  bias = mean(pred - obs) # bias
  rmse = sqrt(ssr / n) # RMSE
  return(
    data.frame(
      'R2' = r2,
      'BIAS' = bias,
      'RMSE' = rmse
    )
  )
}
