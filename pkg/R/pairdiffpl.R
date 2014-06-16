ll.pairdiffpl = function(pars, freq_values, kern, ...) {
  alpha_1 = pars[1L]; l1_1 = pars[2L]; l2_1 = pars[3L]; l3_1 = pars[4L];
  alpha_2 = alpha_1 + pars[5L]; 
  l1_2 = l1_1 + pars[6L]; l2_2 = l2_1 + pars[7L]; l3_2 = l3_1 + pars[8L];  
  
  values1 = freq_values[[1]]$values; values2 = freq_values[[2]]$values
  x = 1:max(1e4, 5*max(c(values1, values2)))
  
  constant1 = sum(kern(l1_1+l2_1*(x-1) + l3_1*(x-1)^2)*dpldis(x, 1, alpha_1))
  constant1 = constant1 + ppldis(max(x)+1, 1, alpha_1, lower.tail=FALSE)

  constant2 = sum(kern(l1_2 + l2_2*(x-1) + l3_2*(x-1)^2)*dpldis(x, 1, alpha_2))
  constant2 = constant2 + ppldis(max(x)+1, 1, alpha_2, lower.tail=FALSE)
  
  like1 = log(kern(l1_1+l2_1*(values1-1) + l3_1*(values1-1)^2)*dpldis(values1, 1, alpha_1)/constant1)
  like2 = log(kern(l1_2+l2_2*(values2-1) + l3_2*(values2-1)^2)*dpldis(values2, 1, alpha_2)/constant2)
  
  sum(c(freq_values[[1]]$freq*like1), c(freq_values[[2]]$freq*like2))
}


get_output_matrix.pairdiffpl = function(pars, N) {
  output = matrix(0, nrow=N, ncol=9)
  colnames(output) = c("ll", "alpha1", paste0("lambda1_", 1:3), "alpha2", paste0("lambda2_", 1:3))
  class(output) = class(pars)
  output
}


check_pars_range.pairdiffpl = function(pars, is_mcmc=TRUE, ...) {
  if(is_mcmc) return(pars[1L] > 1 && all(pars[2:4] > 0) && 
                       pars[1L] + pars[5L] > 1 && all(pars[2:4] + pars[6:8] > 0))
  
  if(pars[1L] < 1 || pars[1L] + pars[5L] < 1  ) stop("First parameter must be greater than 1")
  if(any(pars[2:4] < 0) || any(pars[2:4] + pars[6:8] <= 0)) stop("All scaling parameters must be non-negative")
}


get_prior.pairdiffpl = function(pars, ...) {
  sum(dexp(pars[2:4], rate=1/3, log=TRUE)) + sum(dnorm(pars[5:8], log=TRUE) )
}

#' @export
update_parameters.pairdiffpl = function(pars, cov_mat) {
  pars + mvrnorm(1, numeric(8), cov_mat)
}


check_pars_length.pairdiffpl = function(pars, ...) {

  if(length(pars) != 8L) stop("Must have 8 parameters")
}


#' @export
plot_mcmc.pairdiffpl = function(output, x=1:100) {
  
  op = par(mfrow=c(2, 2), ask=TRUE)
  for(i in 1:8) {
      plot(output[,i+1], type="l", panel.first=grid(), ylab="log-like")
    lines(lowess(output[,i+1]), col=2)
  }
  
  
  par(op)
}

