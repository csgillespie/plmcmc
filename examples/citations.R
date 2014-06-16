##http://physics.bu.edu/~redner/projects/citation/isi.html
library(plmcmc)
library(poweRlaw)
x = readRDS("input/citations.RData")

##Load data
output_diff = readRDS("output/citations_diff.RData")
output_dislnorm = readRDS("output/citations_dislnorm.RData")
output_difflnorm = readRDS(file="output/citations_difflnorm.RData")


#####################
#Diff PL
pars = initialise_pars(pars= output_diff)
cov_mat = cov(log(output_diff[,2:5]))*2.54^2/4

output_diff = mcmc(pars, cov_mat=cov_mat, data=x, N=20000,  kern=exp_cdf, thin=1)
plot_mcmc(output_diff)
plot_fit(x)
y = lines(output_diff, 90000, col=2)

##Log normal fit
pars = initialise_pars(output_dislnorm)
cov_mat = cov(output_dislnorm[,2:3])*2.54^2/2
output_dislnorm = mcmc(pars, cov_mat, data=x, N=20000)

plot_mcmc(output_dislnorm)
plot_fit(x)
y = lines(output_dislnorm, 90000, col=3)

##Diff Log normal fit
pars = initialise_pars(pars=output_difflnorm)
cov_mat = cov(output_difflnorm[,2:6])*2.54^2/5*0.5
output_difflnorm = mcmc(pars, cov_mat, data=x, N=20000)
plot_mcmc(output_difflnorm)
plot_fit(x)
lines(output_difflnorm, 90000, col=4)


bic(x, output_diff)
bic(x, output_difflnorm)
bic(x, output_dislnorm)


############################################################
saveRDS(output_diff, file="output/citations_diff.RData")
saveRDS(output_dislnorm, file="output/citations_dislnorm.RData")
saveRDS(output_difflnorm, file="output/citations_difflnorm.RData")
###########################################
