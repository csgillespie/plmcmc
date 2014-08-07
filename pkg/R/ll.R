ll = function(pars, freq_values, kern,...){
  if(!check_pars_range(pars)) 
    return(-Inf)
  UseMethod("ll")
} 




ll.displ = function(pars, freq_values, ...) {
  freq = freq_values[[1]]$freq; values = freq_values[[1]]$values
  like = dpldis(values, xmin=1, alpha=pars)
  sum(freq*log(like))
}

ll.disexp = local({
  obj = disexp$new(1)
  function(pars, freq_values, ...) {
    freq = freq_values[[1]]$freq; values = freq_values[[1]]$values
    obj$setPars(pars)
    like = dist_pdf(obj, values)
    sum(freq*log(like))
  }
})

ll.dislnorm = local({
  obj = dislnorm$new(1)
  function(pars, freq_values, ...) {
    freq = freq_values[[1]]$freq; values = freq_values[[1]]$values
    obj$setPars(pars)
    like = dist_pdf(obj, values)
    sum(freq*log(like))
  }
})


#' @import poweRlaw
ll.dislnormpl = local({
  obj = dislnorm$new(1)
  function(pars, freq_values, ...) {
    freq = freq_values[[1]]$freq; values = freq_values[[1]]$values
    alpha = pars[1L]
    obj$setPars(pars[2:3])
    prop = pars[4L]; #xmin = pars[5L]
    
    x = 1:max(1e4, 10*max(values))
    xmin = 1
    #like = c(dist_pdf(obj, values[values < xmin], log=TRUE)+log(prop))
    a1 = dist_pdf(obj, values, log=TRUE)+log(prop)
    a2 = dpldis(values, xmin, alpha, log=TRUE)+log(1-prop)
    like = a1 + log(1 + exp(a2-a1))
           
#              
#              dist_pdf(obj, values[values >= xmin], log=TRUE)+log(prop) + 
#                dpldis(values[values >= xmin], xmin, alpha, log=TRUE)+log(1-prop))
    
    sum(freq*like)
  }
})

