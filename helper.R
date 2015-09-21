#' Creates a frequency table
#' Input: vector of numbers and text.
get_freq = function(x, force){
  tab = table(x)
  values = as.numeric(names(tab))
  freq = as.vector(tab)
  m = matrix(c(freq, values), ncol=2, byrow = FALSE)
  colnames(m) = c("freq", "values")
  m
}


exp_cdf = function(theta) 1 - exp(-theta)