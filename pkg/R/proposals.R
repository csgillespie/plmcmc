#' Parameter proposals
#' 
#' @param pars model parameters
#' @param cov_mat covariance matrix
#' @export
#' @importFrom MASS mvrnorm
update_parameters = function(pars, cov_mat) UseMethod("update_parameters")

#'@export
update_parameters.default = function(pars, cov_mat) {
  n_pars = length(pars) 
  pert = mvrnorm(1L, numeric(n_pars), cov_mat)
  exp(log(pars) + pert)
}

#' @export
update_parameters.dislnorm = function(pars, cov_mat) {
  pert = mvrnorm(1L, numeric(2), cov_mat)
  pars + pert
}

#' @export
update_parameters.dislnormpl = function(pars, cov_mat) {
  pars + mvrnorm(1, numeric(4), cov_mat)
}

