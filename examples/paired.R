library(plmcmc)
library(poweRlaw)
options(warn=2)
set.seed(1)
##Load data
data(native_american)
data(us_american)
x = c(native_american$Cas, us_american$Cas)
cut_off = length(native_american$Cas)

##Initialise from past runs
output_diff = readRDS(file="output/native_american_diff.RData")
p1 = initialise_pars(output_diff)
output_diff = readRDS(file="output/us_american_diff.RData")
p2 = initialise_pars(output_diff)

## Initialise
o = readRDS("output/paired_war_diff.RData")
pars = initialise_pars(o)

cov_mat = cov(o[,2:9])/8*2.54
o = mcmc(pars, cov_mat, data=x, N=2000000, thin=20, cut_off=cut_off, kern=exp_cdf)

plot_mcmc(o)


###########################
saveRDS(o, file="output/paired_war_diff.RData")
