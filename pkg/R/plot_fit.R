#' Log-log plot of the data
#' 
#' @param x A vector of data values
#' @param ... Additional arguments passed to the plot function
#' @export
plot_fit = function(x,...) {
  m = displ$new(x)
  plot(m, ...)
}
