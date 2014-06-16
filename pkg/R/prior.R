## Private
get_prior = function(pars, ...) UseMethod("get_prior")

get_prior.displ = function(pars, ...) {
  dexp(pars+1, rate=1, log=TRUE)
}

get_prior.disexp = function(pars, ...) {
  dexp(pars, rate=1/10, log=TRUE)
}
get_prior.dislnorm = function(pars, ...) {
  dnorm(pars[1L], 0, 10, log=TRUE) + dexp(pars[2L], rate=1/10, log=TRUE)
}

##Uniform priors on prop and xmin
get_prior.dislnormpl = function(pars, ...) {
  dexp(pars[1L]+1, rate=1/3, log=TRUE) + #alpha
    dnorm(pars[2L], 0, 10, log=TRUE) + dexp(pars[3L], rate=1/10, log=TRUE) #Log-normal 
    
}
