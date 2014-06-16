#' @export
`[.plmcmc_mat`  = function(x, i, j, ... , drop = TRUE) {
  structure(NextMethod('['), class=class(x), kernel=attr(x, "kern"))
}
