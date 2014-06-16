
## Private
get_output_matrix = function(pars, N) {
  UseMethod("get_output_matrix")
}

get_output_matrix.diff = function(pars, N) {
  output = matrix(0, nrow=N, ncol=5)
  colnames(output) = c("ll", "alpha", paste0("lambda", 1:3))
  class(output) = class(pars)
  output
}

get_output_matrix.difflnorm = function(pars, N) {
  output = matrix(0, nrow=N, ncol=6)
  colnames(output) = c("ll", "p1", "p2", paste0("lambda", 1:3))
  class(output) = class(pars)
  output
}


get_output_matrix.displ = function(pars, N) {
  output = matrix(0, nrow=N, ncol=2)
  colnames(output) = c("ll", "alpha")
  class(output) = class(pars)
  output
}

get_output_matrix.disexp = function(pars, N) {
  output = matrix(0, nrow=N, ncol=2)
  colnames(output) = c("ll", "rate")
  class(output) = class(pars)
  output
}

get_output_matrix.dislnorm = function(pars, N) {
  output = matrix(0, nrow=N, ncol=3)
  colnames(output) = c("ll", "meanlog", "sdlog")
  class(output) = class(pars)
  output
}

get_output_matrix.dislnormpl = function(pars, N) {
  output = matrix(0, nrow=N, ncol=5)
  colnames(output) = c("ll", "alpha", "meanlog", "sdlog", "prop")
  class(output) = class(pars)
  output
}
