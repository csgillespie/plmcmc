library(plmcmc)
library(poweRlaw)

##Load data
data(us_american)
output_diff = readRDS(file="output/us_american_diff.RData")

##Initialise from past runs
pars = initialise_pars(output_diff)
#cov_mat = initialise_cov(output_diff)

cov_mat = cov(output_diff[,2:5])/10
mcmc(pars, cov_mat, data=us_american$Cas, N=100, kern=exp_cdf)
## Infer
#output_diff = mcmc(pars, cov_mat, data=us_american$Cas, N=100, kern=exp_cdf)
data=us_american$Cas;
N=100;
kern=exp_cdf
## Plot
plot_mcmc(output_diff)
plot_fit(us_american$Cas, output_diff)
lines(output_diff)
m = displ$new(us_american$Cas)
(est = estimate_xmin(m))
m$setXmin(est)
lines(m)


###########################
saveRDS(output_diff, file="output/us_american_diff.RData")
