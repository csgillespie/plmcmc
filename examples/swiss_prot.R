library(plmcmc)
library(poweRlaw)
set.seed(1)
##Load data
data(swiss_prot)
output_diff = readRDS(file="output/swiss_prot_diff.RData")

## Initialise from past runs
pars = initialise_pars(output_diff)
cov_mat = cov(log(output_diff[,2:5]))*2.54^2/4

## Infer
output_diff = mcmc(pars, cov_mat, data=swiss_prot$Value, N=50000, thin=5, kern=exp_cdf)

## Plot
plot_mcmc(output_diff)
plot_fit(swiss_prot$Value, output_diff)
lines(output_diff, 1e5, col=2)



###########################
saveRDS(output_diff, file="output/swiss_prot_diff.RData")
