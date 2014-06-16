library(plmcmc)
library(poweRlaw)

##Load data
data(native_american)
output_diff = readRDS(file="output/native_american_diff.RData")

##Initialise from past runs
pars = initialise_pars(output_diff)
cov_mat = initialise_cov(output_diff)

## Infer
output_diff = mcmc(pars, cov_mat, native_american$Cas, N=10000)

##Plot
plot_mcmc(output_diff)
plot_fit(native_american$Cas, output_diff)


###########################
saveRDS(output_diff, file="output/native_american_diff.RData")
