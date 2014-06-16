
#' @export
ll.difflnorm = local({
  obj = dislnorm$new(1)
  function(pars, freq_values, kern, ...) {
    freq = freq_values[[1]]$freq; values = freq_values[[1]]$values
    obj$setPars(pars[1:2])
    l1 = pars[3L]; l2 = pars[4L]; l3 = pars[5L];
    x = 1:1e4
    constant = sum(kern(l1+l2*(x-1) + l3*(x-1)^2)*dist_pdf(obj, x))
    
    like = log(kern(l1+l2*(values-1) + l3*(values-1)^2)*dist_pdf(obj, values)/constant)
    sum(freq*like)
  }
})

#' @export
update_parameters.difflnorm = function(pars, cov_mat) {
  pars + mvrnorm(1, numeric(5), cov_mat)
}

get_prior.difflnorm = function(pars, ...) {
  dnorm(pars[1L], 0, 10, log=TRUE) + 
    dexp(pars[2L], rate=1/10, log=TRUE) + 
    sum(dexp(pars[3:5], rate=1/3, log=TRUE)) 
}

check_pars_range.difflnorm = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(all(pars[2:5] > 0))
  
  if(pars[2L] < 0) stop("First parameter must be greater than 0")
  if(any(pars[3:5] < 0)) stop("All scaling parameters must be non-negative")
}

check_pars_length.difflnorm = function(pars, ...) {
  if(length(pars) != 5L) stop("Must have 5 parameters")
}


