#' X-Y Plotting
#'
#' \code{splot} handels generic 2-d plotting.
#' @param data a two-column data.frame or data.table with x variable in the first column and y variable in the second.
#' @param axis.style axis style, can be one of "i" (default) or "r".
#' @param sec.x logical. If TRUE the axis on the top would be added.
#' @param sec.y logical. If TRUE the axis on the right would be added.
#' @param at1 the points at which x-tick-marks are to be drawn.
#' @param at2 the points at which y-tick-marks are to be drawn.
#' @param at3 the points at which sec.x-tick-marks are to be drawn.
#' @param at4 the points at which sec.y-tick-marks are to be drawn.
#' @param at1.labels x-tick-labels.
#' @param at2.labels y-tick-labels.
#' @param at3.labels sec.x-tick-labels.
#' @param at4.labels sec.y-tick-labels.
#' @param mgp1 mgp for x axis, same as the mgp in \code{\link{par}}.
#' @param mgp2 mgp for y axis, same as the mgp in \code{\link{par}}.
#' @param lwd.axis axis line width.
#' @param bg background color.
#' @param ... other graphical parameters to be passed.
#'
#' @examples
#' splot(cars)
#'
#' @import graphics
#' @import methods

#' @export

splot = function(
    data = NULL,
    axis.style = 'r',
    sec.x = F, sec.y = F,
    at1 = NULL, at2 = NULL, at3 = NULL, at4 = NULL,
    at1.labels = NULL, at2.labels = NULL, at3.labels = NULL, at4.labels = NULL,
    mgp1 = c(3, 0.1, 0), mgp2 = c(3, 0.2, 0),
    lwd.axis = 0.1,
    bg = NULL,
    ...
) {
  olas = par()$las
  otck = par()$tck

  # par ----
  par(las = 1, tck = 0.015)

  # null data ----
  if (is.null(data)) {
    # xlim and ylim must be provided for null data plotting
    if (!hasArg(xlim)) stop('xlim must be provided when plotting NULL data.\n')
    if (!hasArg(ylim)) stop('ylim must be provided when plotting NULL data.\n')
    plot(
      0, type = 'n',
      axes = F, ann = F,
      xaxs = axis.style, yaxs = axis.style,
      ...
    )
  }

  # with data ----
  if (hasArg(data)) {
    if (length(colnames(data)) > 2) stop('data has more than two columns.\n')
    plot(
      data, axes = F,
      xaxs = axis.style, yaxs = axis.style,
      ...
    )
  }

  # axis ----
  # axis 1
  graphics::axis(
    1,
    at = at1,
    labels = at1.labels,
    lwd = lwd.axis,
    mgp = mgp1
  )
  # axis 2
  graphics::axis(
    2,
    at = at2,
    labels = at2.labels,
    lwd = lwd.axis,
    mgp = mgp2
  )
  # axis 3
  if (sec.x) {
    graphics::axis(
      3,
      at = at3,
      labels = at3.labels,
      lwd = lwd.axis,
      mgp = mgp1
    )
  }
  # axis 4
  if (sec.y) {
    graphics::axis(
      4,
      at = at4,
      labels = at4.labels,
      lwd = lwd.axis,
      mgp = mgp2
    )
  }

  # background ----
  if (!is.null(bg)) rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = bg)

  # boundary ----
  box(lwd = lwd.axis)

  # re-par ----
  par(las = olas, tck = otck)
}
