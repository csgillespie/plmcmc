
##Private functions
check_pars_range = function(pars, is_mcmc=TRUE, ...) UseMethod("check_pars_range")



check_pars_range.diffsq =  function(pars, is_mcmc=TRUE, ...) check_pars_range.diff(pars, is_mcmc, ...)


check_pars_range.displ = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars > 1)
  if(pars < 1) stop("Scaling parameter must be greater than 1")
}

check_pars_range.disexp = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars > 0)
  if(pars < 0) stop("Scaling parameter must be greater than 0")
}

check_pars_range.dislnorm = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars[2L] > 1)
  if(pars[2L] < 0) stop("Sd parameter must be greater than 0")
}
check_pars_range.dislnormpl = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars[1L] > 1 && pars[3L] > 0 && pars[4L] > 0 && pars[4L] < 1)
  if(!(pars[1L] > 1 && pars[3L] > 0 && pars[4L] > 0 && pars[4L] < 1)) 
    stop("Pars wrong!")
}


#######################################################
check_pars_length = function(pars, ...) UseMethod("check_pars_length")



check_pars_length.displ = function(pars, ...) {
  if(length(pars) != 1L) stop("Must have 1 parameter")
}

check_pars_length.disexp = function(pars, ...) {
  if(length(pars) != 1L) stop("Must have 1 parameter")
}

check_pars_length.dislnorm = function(pars, ...) {
  if(length(pars) != 2L) stop("Must have 2 parameters")
}

check_pars_length.dislnormpl = function(pars, ...) {
  if(length(pars) != 4L) stop("Must have 4 parameters")
}





