#' Grouped Boxplot at
#'
#' \code{gbat} generates grouped boxplot locations.
#'
#' @param n.group number of groups.
#' @param n.subgroup number of sub-groups.
#' @param gap1 group gap.
#' @param gap2 sub-group gap.
#'
#' @return A list of complete x-axis locations and each group's center location.
#'
#' @seealso
#' \code{\link{gboxplot}}.
#'
#' @examples
#' gbat(5, 2, 2, 1)
#'
#' @export

gbat = function(n.group, n.subgroup, gap1 = 2, gap2 = 1) {
  at.list = list()
  for (i in 1:n.group) {
    at.list[[i]] = (i - 1) * gap1 + seq(1, (1 + gap2), length.out = n.subgroup)
  }
  return(
    list(
      location = Reduce(c, at.list),
      center = Reduce(c, lapply(at.list, mean))
    )
  )
}
