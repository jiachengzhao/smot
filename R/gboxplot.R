#' Grouped Boxplot
#'
#' \code{gboxplot} produces grouped box plot.
#' @param f a formula such as \eqn{y ~ g1:g2}, where y is a numeric vector of data values to be split into groups. Note that \eqn{~ g1 + g2} is equivalent to \eqn{g1:g2}.
#' @param data a data.frame or data.table from which the variables in formula should be taken.
#' @param col box colors.
#' @param alpha color transparency (a number between \code{0} and \code{1}).
#' @param at1 if null (default), even-distributed locations would be generated for the boxes. Otherwise, users should provide a "at list" generated from \code{\link{gbat}} to adjust the positions where the boxes should be drawn.
#' @param at2 the points at which y-tick-marks are to be drawn.
#' @param at1.labels x-tick-labels. Valid only when at1.centered = \code{TRUE}. Otherwise please use \code{names} to change x-tick-labels.
#' @param at2.labels y-tick-labels.
#' @param at1.centered logical. If \code{TRUE} x-axis ticks would be centered.
#' @param mgp1 mgp for x axis, same as the mgp in \code{\link{par}}.
#' @param mgp2 mgp for y axis, same as the mgp in \code{\link{par}}.
#' @param lwd.axis axis line width.
#' @param legend logical. Whether a legend should be added.
#' @param legend.position position of legend.
#' @param legend.label1 legend label for sub-group 1.
#' @param legend.label2 legend label for sub-group 2.
#' @param cex.legend size of legend.
#' @param x.intersp horizontal spacing between legend item and text.
#' @param y.intersp vertical spacing between legend items.
#' @param inset inset distance from the margins as a fraction of the plot region when legend is placed by keyword.
#' @param ... other graphical parameters to be passed.
#'
#' @return List with components same as \code{\link{boxplot}}.
#'
#' @seealso
#' \code{\link{gbat}}.
#'
#' @import grDevices
#' @import stats
#'
#' @examples
#' opar = par(no.readonly = TRUE)
#' par(cex.axis = 0.9, mai = c(0.5, 0.4, 0.3, 0.2), mfrow = c(2, 2), oma = c(1, 5, 2, 5))
#'
#' # base r boxplot ----
#' boxplot(
#'   len ~ supp:dose, data = ToothGrowth,
#'   boxwex = 0.5, col = c('red', 'green'),
#'   xlab = '', ylab = 'tooth length',
#'   ylim = c(0, 35), yaxs = 'i',
#'   sep = ':',
#'   main = 'base r boxplot'
#' )
#'
#' # gboxplot with defaul at and legend ----
#' gboxplot(
#'   len ~ supp:dose, data = ToothGrowth,
#'   boxwex = 0.5, col = c('red', 'green'), alpha = 0.5,
#'   xlab = '', ylab = 'tooth length',
#'   ylim = c(0, 35), yaxs = 'i',
#'   sep = ':',
#'   main = 'gboxplot with defaul at and legend',
#'   legend = TRUE, legend.position = 'bottomright', legend.label1 = 'OJ', legend.label2 = 'VC', cex.legend = 0.8,
#'   x.intersp = 0.8, y.intersp = 0.8
#' )
#'
#' # gboxplot with adjusted at ----
#' n.group = 3
#' n.subgroup = 2
#' # adjusted box positions
#' myat = gbat(n.group, n.subgroup, 1.2, 0.35)
#' gboxplot(
#' len ~ supp:dose, data = ToothGrowth,
#' boxwex = 0.2, col = c('red', 'green'), alpha = 0.5,
#' xlab = '', ylab = 'tooth length',
#' at1 = myat,
#' ylim = c(0, 35), yaxs = 'i',
#' sep = ':',
#' main = 'gboxplot with adjusted at',
#' legend = TRUE, legend.position = 'bottomright', legend.label1 = 'OJ', legend.label2 = 'VC', cex.legend = 0.8,
#' x.intersp = 0.8, y.intersp = 0.8
#' )
#'
#' # gboxplot with adjusted at and centered ticks ----
#' gboxplot(
#'   len ~ supp:dose, data = ToothGrowth,
#'   boxwex = 0.2, col = c('red', 'green'), alpha = 0.5,
#'   xlab = '', ylab = 'tooth length',
#'   at1 = myat, at1.labels = c(0.5, 1, 2), at1.centered = TRUE,
#'   ylim = c(0, 35), yaxs = 'i',
#'   sep = ':',
#'   main = 'gboxplot with adjusted at and centered ticks',
#'   legend = TRUE, legend.position = 'bottomright', legend.label1 = 'OJ', legend.label2 = 'VC', cex.legend = 0.8,
#'   x.intersp = 0.8, y.intersp = 0.8
#' )
#' par(opar)
#'
#' @export

gboxplot = function(
    f, data,
    col = c('green', 'red'), alpha = 1,
    at1 = NULL, at2 = NULL,
    at1.labels = NULL, at2.labels = NULL,
    at1.centered = FALSE,
    mgp1 = c(3, 0.2, 0),
    mgp2 = c(3, 0.25, 0),
    lwd.axis = 0.1,
    legend = FALSE, legend.position = 'topright', legend.label1 = 'Sub-group 1', legend.label2 = 'Sub-group 2', cex.legend = 1,
    x.intersp = 1, y.intersp = 1,
    inset = c(0, 0),
    ...
) {
  olas = par()$las
  omgp = par()$mgp
  otck = par()$tck

  # base boxplot ----
  if (is.null(at1)) {
    par(las = 1, tck = 0.015, mgp = mgp1)
    bp = boxplot(
      as.formula(f),
      data = data, yaxt = 'n',
      col = adjustcolor(col, alpha.f = alpha),
      ...
    )
  } else {
    if (at1.centered == TRUE) {
      par(las = 1, tck = 0.015)
      bp = boxplot(
        as.formula(f),
        data = data, axes = FALSE,
        col = adjustcolor(col, alpha.f = alpha),
        at = at1$location,
        ...
      )
      axis(1, at = at1$center, labels = at1.labels, lwd = lwd.axis, mgp = mgp1)
    } else {
      par(las = 1, tck = 0.015, mgp = mgp1)
      bp = boxplot(
        as.formula(f),
        data = data, yaxt = 'n',
        col = adjustcolor(col, alpha.f = alpha),
        at = at1$location,
        ...
      )
    }
  }

  # axis ----
  # axis 2
  axis(2, at = at2, labels = at2.labels, lwd = lwd.axis, mgp = mgp2)

  # boundary ----
  box(lwd = lwd.axis)

  # legend ----
  if (legend) {
    legend(
      legend.position,
      legend = c(
        legend.label1,
        legend.label2
      ),
      fill = adjustcolor(col, alpha.f = alpha),
      cex = cex.legend, bty = 'n',
      x.intersp = x.intersp, y.intersp = y.intersp,
      inset = inset
    )
  }

  # re-par ----
  par(las = olas, tck = otck, mgp = omgp)

  # return ----
  invisible(bp)
}
