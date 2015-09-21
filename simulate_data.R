source("truncated_poisson.R")


add_noise = function(y,p) {
  
  y_pois = rtpois(y, y)  
  possible= which(y_pois > 2)
  N = rbinom(1, size = length(possible), prob = p)
  
  indx = sample(possible, size=N)
  rounded = y_pois
  rounded[indx] = round(y_pois[indx]/5)*5
  data.frame(y, y_pois, rounded)
}

simulate_data = function(n, pars, seed=1){
  set.seed(seed)
  kern = function(theta) 1 - exp(-theta)
  alpha = pars[1L]; l1 = pars[2L]; l2 = pars[3L]; p = pars[4L]
  
  x = rpldis(n, 1, alpha)
  
  prob_obs = kern(l1 + l2*(x-1))
  total = data.frame(x)
  total$observed = prob_obs > runif(n)
  y = x[total$observed]
  
 
  l = list()
  l[[1]] = total
  l[[2]] = add_noise(y, p)
  l[[2]] = l[[2]][with(l[[2]], order(rounded)), ]
  
  
  l
}





# 
# x = dd$z1
