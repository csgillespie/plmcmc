#' @export
ll.diff = function(pars, freq_values, kern, ...) {
  if(!check_pars_range(pars)) return(-Inf)
  
  alpha = pars[1L]; l1 = pars[2L]; l2 = pars[3L]; l3 = pars[4L];
  freq = freq_values[[1]]$freq; values = freq_values[[1]]$values

  x = 1:1e4
  constant = sum(kern(l1 + l2*(x-1) + l3*(x-1)^2)*dpldis(x, 1, alpha))
  constant = constant + ppldis(max(x)+1, 1, alpha, lower.tail=FALSE)
    
  like = log(kern(l1 + l2*(values-1) + l3*(values-1)^2)* dpldis(values, 1, alpha)/constant)
  sum(freq*like)
}

#' @export
update_parameters.diff = function(pars, cov_mat) {
  pert = mvrnorm(1L, numeric(4), cov_mat)
  exp(log(pars) + pert)
}

#' @export
get_prior.diff = function(pars, ...) {
  dunif(pars[1], 1, 8, log=TRUE) + 
    sum(dexp(pars[2:4], rate=1/3, log=TRUE))
}

check_pars_range.diff = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars[1L] > 1 && all(pars[2:4] > 0))
  
  if(pars[1L] < 1) stop("First parameter must be greater than 1")
  if(any(pars[3:4] < 0)) stop("All scaling parameters must be non-negative")
}

check_pars_length.diff = function(pars, ...) {
  if(length(pars) != 4L) stop("Must have 4 parameters")
}

