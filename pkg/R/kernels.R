inv_logit = function(theta, b=20) 1/(1 + exp(b -theta))
inv_cloglog = function(theta) 1 - exp(-exp(theta))
inv_probit = function(theta) pnorm(theta)
inv_cauchit = function(theta) 0.5 + atan(theta)/pi


#' Unit exponential CDF
#' 
#' The Unit exponential CDF function. Used as a kernel in the \code{mcmc} function.
#' @param theta Should be positive
#' @export
exp_cdf = function(theta) 1 - exp(-theta)
