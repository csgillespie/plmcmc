#' Xmin inference
#' 
#' @param thetas Matrix of parameters. Typically posterior samples
#' @param x Values of x to scan
#' @param upper arg min cut-off. Default 0.99
#' @export
get_xmins = function(thetas, x, upper =0.99) {
  cut_off = matrix(0, ncol=length(x), 
                   nrow=nrow(thetas))
  
  for(i in seq_along(x)) {
    cut_off[,i] = apply(thetas, 1, function(theta) exp_cdf(sum(theta*c(1, x[i], x[i]^2))))
  }
  
  co = cut_off >= upper
  xmins = apply(co, 1, function(i) min(which(i)))
  return(xmins)
}