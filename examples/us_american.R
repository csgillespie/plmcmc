library(plmcmc)
library(poweRlaw)

##Load data
data(us_american)
output_diff = readRDS(file="output/us_american_diff.RData")

##Initialise from past runs
pars = initialise_pars(output_diff)
cov_mat = initialise_cov(output_diff)

## Infer
output_diff = mcmc(pars, cov_mat, us_american$Cas, N=10000)

## Plot
plot_mcmc(output_diff)
plot_fit(us_american$Cas, output_diff)



###########################
saveRDS(output_diff, file="output/us_american_diff.RData")
