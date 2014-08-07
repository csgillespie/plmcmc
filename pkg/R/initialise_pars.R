#' Initialise parameter values for the MCMC algorithm
#' 
#' @param pars A vector of initial parameter values
#' @param type A string indicating the model to be fitted
#' @export
initialise_pars = function(pars, 
                           type=c("displ", "dislnorm", "disexp", 
                                  "diff", "difflnorm", "diffsq", 
                                  "diffxmax", "difflnormxmax",
                                  "dislnormpl", 
                                  "pairdiffpl")) {
  if(is.matrix(pars)) {
    cl = class(pars)
    cl = cl[cl != "plmcmc_mat"]
    pars = pars[nrow(pars), 2:ncol(pars)]
    class(pars) = cl
  } else {
    cl = match.arg(type)
    class(pars) = cl
    if(type =="diffxmax") class(pars) = c(class(pars), "diff")
    if(type =="difflnormxmax") class(pars) = c(class(pars), "difflnorm")
  }
  
  check_pars_length(pars)
  check_pars_range(pars, is_mcmc=FALSE)
  pars
}
