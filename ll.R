## Biv Lognormal density
biv_lognormal =  function(x, y, mu_1, mu_2, sigma_1, sigma_2, tau) 
{
  A = (log(x) - mu_1)/sigma_1
  B = (log(y) - mu_2)/sigma_2
  1 / (2*pi*sigma_1 * sigma_2*sqrt(1-tau^2)*x*y) * 
    exp(-(A^2 + B^2 - 2*tau*A*B)/(2*(1-tau^2)))
}

## Rounding/heaping ll
ll_rounded = function(x_prop, pars, obser) {
  p = pars[4]  
  if(p < 0 || p > 1) return(-Inf)
  true = x_prop#[,"true"]
  
  ## Observations 1, 2
  less_than_3 = (obser  < 2.5)
  (probs1 = sum(dtpois(obser[less_than_3], true[less_than_3], log=TRUE)))
  
  ## Observations 3, 4, 6, 7, 8, 9, 11, ...
  non_rounded = which((obser %% 5) > 0 & obser > 2.5)
  # probs2 = log(sum(dtpois(obser[non_rounded], true[non_rounded]))) + log(1-p)
  
  (probs2 = sum(dtpois(obser[non_rounded], true[non_rounded], log=TRUE)+log(1-p)))
  
  ## Observations 5, 10, 15, ..  
  rounded = which((obser %% 5) == 0)
  rounded_obs = obser[rounded]
  probs3 = 0
  
  for(i in 1:length(rounded)){
    #message(i, " ", probs3)  
    obs = rounded_obs[i]
    probs3 = probs3 + 
      log(sum(dtpois(obs + -2:2, true[rounded[i]]))*p + dtpois(obs, true[rounded[i]])*(1-p))
  }
  
  probs1+probs2 + probs3
}


#' ll for pl component
ll_pl = function(x_prop, pars){
  kern = function(theta) 1 - exp(-theta)
  m = get_freq(x_prop)
  alpha = pars[1L]; l1 = pars[2L]; l2 = pars[3L]
  freq = m[,"freq"]; values = m[, "values"]
  
  x = 1:1e4
  constant = sum(kern(l1 + l2*(x-1))*dpldis(x, 1, alpha))
  constant = constant + ppldis(max(x)+1, 1, alpha, lower.tail=FALSE)
  
  like = log(kern(l1 + l2*(values-1))* dpldis(values, 1, alpha)/constant)
  sum(freq*like)
}


ll = function(x_prop, pars, obser) {
  us = x_prop[x_prop$force == "us",]
  nat = x_prop[x_prop$force == "nat",]
  if(is.infinite(get_prior(pars))) {
    ll = -Inf
  } else {
    ll = ll_pl(us[,"true"], pars[1:4]) +  
      ll_rounded(us[,"true"], pars[1:4], obser[obser$force=="us", "cas"]) + 
      ll_pl(nat[,"true"], pars[5:8]) +  
      ll_rounded(nat[,"true"], pars[5:8], obser[obser$force=="nat", "cas"])
  }
}

## Update latent state
## Select ten values and perturb
update_x = function(x_prop, obser) {
  obser = obser$cas
  x_true_cur = x_prop[,"true"]
  x_true_prop = x_true_cur
  size = 10
  indx = sample(1:length(obser), size=size)
  x_true_prop[indx] = rtpois(size, obser[indx])
  trans_prob = sum(dtpois(x_true_cur, obser, log=TRUE)) - sum(dtpois(x_true_prop, obser, log=TRUE))
  x_prop[, "true"] = x_true_prop
  attr(x_prop, "trans") = trans_prob
  x_prop
}



#' Random walk on parameters
update_parameters = function(pars, cov_mat) {
  c1 = cov_mat[[1]]
  pert = mvrnorm(1L, numeric(3), c1[1:3, 1:3])
  pars[1:3] = exp(log(pars[1:3]) + pert)
  pars[4] = pars[4] + rnorm(1, sd = c1[4, 4])
  
  c2 = cov_mat[[2]]  
  pert = mvrnorm(1L, numeric(3), c2[1:3, 1:3])
  pars[5:7] = exp(log(pars[5:7]) + pert)
  pars[8] = pars[8] + rnorm(1, sd = c2[4, 4])
  
  pars
}

#' Prior
get_prior = function(pars, ...) {
  mu_1 = 0;
  mu_2 = -3
  sigma_1 = 1
  sigma_2 = 2
  tau = sigma_1*sigma_2*0.3
  (tau= tau/sigma_1/sigma_2)

  dunif(pars[1], 1, 8, log=TRUE) + #alpha1
    dunif(pars[5], 1, 8, log=TRUE) + #alpha2
    dunif(pars[4], 0, 1, log=TRUE) +#q1
    dunif(pars[8], 0, 1, log=TRUE) #+#q2
    log(biv_lognormal(pars[2], pars[6], mu_1, mu_2, sigma_1, sigma_2, tau)) + 
    log(biv_lognormal(pars[3], pars[7], mu_1, mu_2, sigma_1, sigma_2, tau)) 
}

check_pars_range = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(!(is.infinite(get_prior(pars))))
  if(pars[1L] < 1) stop("First parameter must be greater than 1")
  if(any(pars[2:4] < 0)) stop("All scaling parameters must be non-negative")
  
}

check_pars_length = function(pars, ...) {
  if(length(pars) != 8L) stop("Must have 8 parameters")
}

