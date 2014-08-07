get_freq_and_values = function(x, ...) {
  args = as.list(match.call())
  if(!any("cut_off" %in% names(args))) {
    cut_off = NULL
  }
  
  ## Extend cut_off for subsetting
  cut_off = c(1, cut_off, length(x))
  l = vector("list", length(cut_off)-1L)
  
  for(cut in seq_along(cut_off)[-1]) {
    x_cut = x[cut_off[cut-1]:cut_off[cut]]
    l[[cut-1]]$values = as.numeric(names(table(x_cut)))
    l[[cut-1]]$freq = as.vector(table(x_cut))
  }
  l
}


#' Main MCMC function
#' 
#' Function used to perform parameter inference
#' @param pars Initial parameter values
#' @param cov_mat variance-covariance matrix used in the Normal random walk
#' @param data The data set (a vector)
#' @param N Number of MCMC iterations
#' @param thin Should the MCMC output be thinned
#' @param kern default \code{NULL}. The difference kernel to be used. When fitting a standard log normal or 
#' power law distribution, we don't need a difference kernel. Otherwise, a function should be passed. For example,
#' \code{exp_cdf}
#' @param verbose default \code{TRUE}. On verbose mode, 1 in 100 iterations and the acceptance 
#' rate will be printed to the screen.
#' @param ... Additional arguments that will be passed to the likelihood function.
#' @return A matrix
#' @export
mcmc = function(pars, cov_mat, data, N, 
                thin=1, kern=NULL, verbose=TRUE, ...) {
  pars_cur = pars
  freq_values = get_freq_and_values(data, ...)
  log_ll_cur = ll(pars, freq_values, kern=kern, ...)
  prior_cur = get_prior(pars_cur)
  
  output = get_output_matrix(pars, N/thin)
  output[1,] = c(log_ll_cur, pars_cur)
  accept = 0
  
  for(i in 2:N) {
    pars_prop = update_parameters(pars_cur, cov_mat)
    log_ll = ll(pars_prop, freq_values, kern=kern, ...)  
    prior = get_prior(pars_prop)
    
    ratio = log_ll - log_ll_cur + prior - prior_cur
    if(is.finite(ratio) && ratio > log(runif(1))) {
      log_ll_cur = log_ll
      pars_cur = pars_prop
      prior_cur = prior
      accept = accept + 1
    } 
    
    if(!(i %% thin))
      output[i/thin,] = c(log_ll_cur, pars_cur)  
    
    if(verbose && !(i %% ceiling(N/100)))
      message(i, ": ", paste(c(signif(pars_cur, 4), signif(prior),signif(log_ll)), collapse=" "))
  }
  if(verbose) message("acceptance = ", signif(accept/N*100, 2), "%")
  class(output) = c("plmcmc_mat", class(output))
  attr(output, "kern") = kern
  
  if(verbose && require("beepr")) beep(6)
  return(output)
}

