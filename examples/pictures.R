library(plmcmc)

#Load previous MCMC results
output_diff = readRDS("output/pics_diff.RData")
output_dislnorm = readRDS(file="output/pics_dislnorm.RData")
output_difflnorm = readRDS(file="output/pics_difflnorm.RData")

## Read in data
dd = read.table("input/pics_ui/out.pics_ui", skip=2)
x = table(dd$V1)


#####################
#Diff PL
pars = initialise_pars(pars= output_diff)
cov_mat = cov(log(output_diff[,2:5]))*2.54^2/4
output_diff = mcmc(pars, cov_mat, data=x, N=20000, kern=exp_cdf)

plot_mcmc(output_diff)
plot_fit(x)
y = lines(output_diff, 1e6, col=2)

##Log normal fit
pars = initialise_pars(output_dislnorm)
cov_mat = cov(output_dislnorm[,2:3])*2.54^2/2
output_dislnorm = mcmc(pars, cov_mat, data=x, 20000, thin=1)
plot_mcmc(output_dislnorm)
plot_fit(x)
lines(output_dislnorm, 1e5, col=2)

##Diff Log normal fit
pars = initialise_pars(pars=output_difflnorm)
cov_mat = cov(output_difflnorm[,2:6])*2.54^2/5*0.5
output_difflnorm = mcmc(pars, cov_mat, data=x, 200000, thin=10, kern=exp_cdf)
plot_mcmc(output_difflnorm)

plot_fit(x)
lines(output_difflnorm, 1e5, col=2)

######################
###ALL
plot_fit(x)

lines(output_diff,1e5, col=2)
lines(output_dislnorm,1e5, col=3)
lines(output_difflnorm, 1e5, col=4)

bic(x, output_diff)
bic(x, output_difflnorm)
bic(x, output_dislnorm)

#####################################################
saveRDS(output_diff, file="output/pics_diff.RData")
saveRDS(output_dislnorm, file="output/pics_dislnorm.RData")
saveRDS(output_difflnorm, file="output/pics_difflnorm.RData")








###########################################
##Dislnormpl fit
#output_dislnormpl = output_dislnormpl[,-6]
# pars = initialise_pars(output_dislnormpl)
# cov_mat = initialise_cov(output_dislnormpl, scale=2)
# output_dislnormpl = mcmc(pars, cov_mat, x, 50000, thin=)
# 
# plot_mcmc(output_dislnormpl)
# plot_fit(x)
# lines(output_dislnormpl,1e5, col=2)
#saveRDS(output_dislnormpl, file="output/pics_dislnormpl.RData")