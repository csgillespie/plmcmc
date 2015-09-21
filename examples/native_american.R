library(plmcmc)
library(poweRlaw)

##Load data
data(native_american)
output_diff = readRDS(file="output/native_american_diff.RData")

##Initialise from past runs
pars = initialise_pars(output_diff)
#cov_mat = initialise_cov(output_diff)
cov_mat = cov(output_diff[, 2:5])/4

## Infer
output_diff = mcmc(pars, cov_mat, native_american$Cas, N=10000, kern=exp_cdf)

##Plot
plot_mcmc(output_diff)
plot_fit(native_american$Cas)

lines(output_diff, xmax=1e4, col=2)

m = displ$new(native_american$Cas)
(est = estimate_xmin(m))
m$setXmin(est)
lines(m)
###########################
saveRDS(output_diff, file="output/native_american_diff.RData")
