#' Bayesian information criterion
#' 
#' Function for calculating Bayesian information criterion (BIC)
#' for a single model. The formula is -2*log-like + npar*log(n),
#' where n is the sample size.
#' 
#' @param x the data set
#' @param output The output from the \code{mcmc} function
#' @export
bic = function(x, output) {
  l = get_means(output)[-1L]
  class(l) = class(output)
  
  freq_values = get_freq_and_values(x)
  -2*ll(l, freq_values, kern=attr(output, "kern")) + length(l)*log(length(x))
}
