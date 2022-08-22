#' Time Series Completion
#'
#' \code{ts.complete} fill the missing data in a time series.
#'
#' @import tidyr
#' @import zoo
#'
#' @param data a data table with the first column containing dates and the second column containing values.
#' @param by increment of the sequence (e.g., 'day', 'month' or numbers).
#' @param fill logical. If TRUE, NA values will be filled using a seasonal Kalman filter.
#' @param col.date date column.
#' @param col.variable variable column.
#' @param ... other arguments (e.g., frequency) passed to \code{\link{ts}}.
#'
#' @return A time-series object.
#'
#' @examples
#' dt = data.frame(
#'   date = sort(
#'     sample(
#'       seq(
#'         as.Date('2000/05/04'),
#'         as.Date('2000/06/30'),
#'         by = 'day'
#'       ),
#'       30
#'     )
#'   ),
#'   value = 1:30 + runif(30)
#' )
#' unfilled = ts.complete(data = dt, by = 'day', fill = FALSE, col.variable = 'value', frequency = 12)
#' filled = ts.complete(data = dt, by = 'day', col.variable = 'value', frequency = 12)
#' opar = par(no.readonly = TRUE)
#' par(mfrow = c(1, 2))
#' plot(unfilled)
#' plot(filled)
#' par(opar)
#'
#' @export
#'
ts.complete = function(data, by, fill = TRUE, col.date = 'date', col.variable, ...) {
  tb = tidyr::complete(
    data,
    date = seq(
      min(data[, col.date]),
      max(data[, col.date]),
      by = by
    ),
    fill = list(value = NA)
  )
  start_y = format(as.Date(min(tb$date), format = '%d/%m/%Y'), '%Y')
  start_m = format(as.Date(min(tb$date), format = '%d/%m/%Y'), '%m')
  if(fill == TRUE) {
    return(
      zoo::na.StructTS(
        stats::ts(
          as.numeric(as.matrix(tb[, col.variable])),
          start = c(start_y, start_m),
          ...
        )
      )
    )
  } else {
    return(
      stats::ts(
        as.numeric(as.matrix(tb[, col.variable])),
        start = c(start_y, start_m),
        ...
      )
    )
  }
}
