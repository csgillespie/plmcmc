#' Fitted lines
#' 
#' Adds the fitted to a plot
#' @param x Output from the \code{mcmc} function
#' @param xmax Maximum x value to plot 
#' @param n The number of samples from \code{x} to plot
#' @param ... Additional lines arguments
#' @method lines plmcmc_mat
#' @export
lines.plmcmc_mat = function(x, xmax=1e4, n=NULL, ...) {
  x_mcmc = x[,-1, drop=FALSE]
  if(is.null(n)) {
    x_mcmc = t(as.matrix(colMeans(x_mcmc)))
    class(x_mcmc) = class(x)
    attr(x_mcmc, "kern") = attributes(x)$kern
    n = 1
    
  }
  i = 1
  for(i in 1:n) {
    s = sample(1:nrow(x_mcmc), 1)
    x = x_mcmc[s,, drop=FALSE]
    NextMethod()
  }
  
  return(x)
}

#' @rdname lines.plmcmc_mat
#' @method lines diff
#' @export
lines.diff = function(x, xmax=1e4, n,...) {
  kern = attr(x, "kern")
  x_seq = 1:xmax
  pars = x
  alpha = pars[1L]; l1 = pars[2L]; l2 = pars[3L]; l3 = pars[4L];
  constant = sum(kern(l1 + l2*(x_seq-1) + l3*(x_seq-1)^2)*dpldis(x_seq, 1, alpha))
  constant = constant + ppldis(max(x_seq)+1, 1, alpha, lower.tail=FALSE)
  
  probs = kern(l1 + l2*(x_seq-1) + l3*(x_seq-1)^2)*dpldis(x_seq, 1, alpha)
  probs = probs/constant
  
  ## Add on the part of the pdf not covered in the search space
  ## Then drop it from plotting
  p = c(probs, 1 - sum(probs))
  cdf = rev(cumsum(rev(p)))[-(xmax+1)]
  lines(x_seq, cdf)
  
  lines(x_seq, cdf, ...)

  return(invisible(data.frame(x=x_seq, y=cdf)))
}

#' @rdname lines.plmcmc_mat
#' @method lines difflnorm
#' @export
lines.difflnorm = function(x, xmax, n,...) {
  x_seq = 1:xmax
  obj = dislnorm$new(x_seq)
  obj$setPars(x[1:2])
  
  l1 = x[3]; l2 = x[4]; l3 = x[5]
  
  constant = sum((1-exp(-(l1 + l2*(x_seq-1) + l3*(x_seq-1)^2 )))*dist_pdf(obj, x_seq))
  constant = constant + dist_cdf(obj, q=xmax + 1, lower_tail=FALSE)
  
  probs = (1-exp(-(l1 + l2*(x_seq-1) + l3*(x_seq-1)^2))) *dist_pdf(obj, x_seq)
  probs = probs/constant
  
  cs = 1-cumsum(probs)
  cdf = cs/cs[1]##Normalise
  
  lines(x_seq, cdf, ...)
  return(invisible(data.frame(x=x_seq, y=cdf)))
}

#' @rdname lines.plmcmc_mat
#' @method lines dislnorm
#' @export
lines.dislnorm = function(x, xmax, n, ...) {
  x_seq = floor(seq(1, xmax, length.out=4))
  obj = dislnorm$new(x_seq)
  obj$setPars(x)
  return(lines(obj, ...))
}

#' @rdname lines.plmcmc_mat
#' @method lines displ
#' @export
lines.displ = function(x, xmax, n, ...) {
  x_seq = floor(seq(1, xmax, length.out=4))
  obj = displ$new(x_seq)
  obj$setPars(x)
  lines(obj, ...)
}

#' @rdname lines.plmcmc_mat
#' @method lines dislnormpl
#' @export
lines.dislnormpl = function(x, xmax, n, ...) {
  ## Plot the data
  x_seq = 1:xmax
  obj = dislnorm$new(x_seq)
  obj$setPars(x[2:3])
  
  alpha = x[1L]; prop = x[4L]; 
  probs = dist_pdf(obj, x_seq)*prop + dpldis(x_seq, 1, alpha)*(1-prop)
  
  cs = 1-cumsum(probs)
  cdf = cs/cs[1]##Normalise
  
  lines(x_seq, cdf, ...)
  return(invisible(data.frame(x=x_seq, y=cdf)))
}



