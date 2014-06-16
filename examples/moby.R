library(plmcmc)
library(poweRlaw)
data(moby)
set.seed(1)

##Load data
output_diff = readRDS(file="output/moby_diff.RData")
pars = initialise_pars(pars= output_diff)
cov_mat = cov(log(output_diff[,2:5]))*2.54^2/4

output_diff = mcmc(pars =pars, cov_mat=cov_mat,
                   N=10000, data=moby, thin=1, 
                   kern=exp_cdf)


plot_mcmc(output_diff)
plot_fit(moby)
lines(output_diff, xmax=1e4, col=2)


############################
saveRDS(output_diff, file="output/moby_diff.RData")
