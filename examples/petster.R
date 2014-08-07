## http://konect.uni-koblenz.de/networks/petster-hamster
## Download petster-hamster.tar.bz2 and extract
library(plmcmc)
set.seed(1)
## Read in data
dd = read.table("input/petster-hamster/out.petster-hamster", skip=2)
x = table(dd$V1)
output_diff = readRDS(file="output/petster_diff.RData")
cov_mat=cov(log(output_diff[,2:5]))*2.54^2/4

pars = initialise_pars(pars= output_diff)

output_diff = mcmc(pars =pars, cov_mat=cov_mat,
                   N=100000, data=x, thin=10,
                   kern=exp_cdf)
plot_mcmc(output_diff)

plot_fit(x, ylim=c(1e-5, 1), xlim=c(1, 1e3))
lines(output_diff, xmax=1e3, col=4)

###########################
saveRDS(output_diff, file="output/petster_diff.RData")


