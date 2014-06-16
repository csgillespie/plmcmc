get_means = function(output) {
    colMeans(output)
}


#' @export
plot_mcmc = function(output, ...) UseMethod("plot_mcmc")
#' @export
plot_mcmc.diff = function(output, x=1:100) {
  
  op = par(mfrow=c(3, 2))
  on.exit(par(op))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab=expression(alpha))
  lines(lowess(output[,2]), col=2)
  
  plot(output[,3], type="l", panel.first=grid(), ylab=expression(lambda[1]))
  lines(lowess(output[,3]), col=2)
  
  plot(output[,4], type="l", panel.first=grid(), ylab=expression(lambda[2]))
  lines(lowess(output[,4]), col=2)
  
  plot(output[,5], type="l", panel.first=grid(), ylab=expression(lambda[3]))
  lines(lowess(output[,5]), col=2)
  
  l = colMeans(output) 
  plot(x, attributes(output)$kern(l[3]+l[4]*(x-1) + l[5]*(x-1)^2),
       ylim=c(0, 1), panel.first=grid(), 
       ylab="Inv logit", type="l")
  
 
}
#' @export
plot_mcmc.difflnorm = function(output, x=1:100) {
  
  op = par(mfrow=c(3, 2))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab="P1")
  lines(lowess(output[,2]), col=2)
  
  plot(output[,3], type="l", panel.first=grid(), ylab="P2")
  lines(lowess(output[,3]), col=2)
  
  plot(output[,4], type="l", panel.first=grid(), ylab=expression(lambda[1]))
  lines(lowess(output[,4]), col=2)
  
  plot(output[,5], type="l", panel.first=grid(), ylab=expression(lambda[2]))
  lines(lowess(output[,5]), col=2)
  
  plot(output[,6], type="l", panel.first=grid(), ylab=expression(lambda[3]))
  lines(lowess(output[,6]), col=2)
  
  par(op)
}


#' @export
plot_mcmc.diffsq = function(output, x=1:100) {
  
  op = par(mfrow=c(3, 2))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab=expression(alpha))
  lines(lowess(output[,2]), col=2)
  
  plot(output[,3], type="l", panel.first=grid(), ylab=expression(lambda[1]))
  lines(lowess(output[,3]), col=2)
  
  plot(output[,4], type="l", panel.first=grid(), ylab=expression(lambda[2]))
  lines(lowess(output[,4]), col=2)
  
  plot(output[,5], type="l", panel.first=grid(), ylab=expression(lambda[3]))
  lines(lowess(output[,5]), col=2)
  
  l = get_means(output) 
  plot(x,
       (1-exp(-(l[3] +l[4]*log(x) + l[5]*(x-1)))), type="l",
       ylim=c(0, 1), panel.first=grid(), 
       ylab=expression(1-exp(-(lambda[1]+lambda[2]*x+lambda[3]*x^2))))
  
  par(op)
}

#' @export
plot_mcmc.displ = function(output) {
  
  op = par(mfrow=c(1, 2))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab=expression(alpha))
  lines(lowess(output[,2]), col=2)
  
  par(op)
}

#' @export
plot_mcmc.dislnorm = function(output) {
  
  op = par(mfrow=c(1, 3))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab="Par 1")
  lines(lowess(output[,2]), col=2)
  
  plot(output[,3], type="l", panel.first=grid(), ylab="Par 2")
  lines(lowess(output[,3]), col=2)
  
  par(op)
}

#' @export
plot_mcmc.dislnormpl = function(output) {
  
  op = par(mfrow=c(2, 3))
  
  plot(output[,1], type="l", panel.first=grid(), ylab="log-like")
  lines(lowess(output[,1]), col=2)
  
  plot(output[,2], type="l", panel.first=grid(), ylab=expression(alpha))
  lines(lowess(output[,2]), col=2)
  
  plot(output[,3], type="l", panel.first=grid(), ylab="log mu")
  lines(lowess(output[,3]), col=2)
  
  plot(output[,4], type="l", panel.first=grid(), ylab="log sd")
  lines(lowess(output[,4]), col=2)
  
  plot(output[,5], type="l", panel.first=grid(), ylab="prop")
  lines(lowess(output[,5]), col=2)
  
 
  
  par(op)
}



