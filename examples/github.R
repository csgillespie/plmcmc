## https://github.com/blog/466-the-2009-github-contest
library(plmcmc)
dd = read.table("input/github/data.txt", sep=":", header=FALSE)
x = as.vector(table(dd$V1))
set.seed(1)
############################################################
#Load previous MCMC results
output_diff = readRDS(file="output/github_diff.RData")
############################################################

cov_mat=cov(log(output_diff[,2:5]))*2.54^2/4
pars = initialise_pars(pars= output_diff)
output_diff = mcmc(pars =pars, cov_mat=cov_mat,
                   N=10000, data=x, thin=1,
                   kern=exp_cdf)
plot_mcmc(output_diff)
plot_fit(x)
lines(output_diff, xmax=1e3, col=2)

############################################################
saveRDS(output_diff, file="output/github_diff.RData")
############################################################
