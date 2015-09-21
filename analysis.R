## Load packages
library("poweRlaw")
library("MASS")

## Load data sets
data("native_american", package="poweRlaw")
data("us_american", "poweRlaw")

## Load functions
source("ll.R")
source("mcmc.R")
source("helper.R")
source("truncated_poisson.R")
## Initialise from past runs
output_diff = readRDS(file="output/combined_mcmc.RData")[[1]]
colMeans(output_diff)
o1 = output_diff
##Initialise from past runs
## cov_mat used in the random walks
(p1 = colMeans(output_diff[,2:5]))
cov_mat = suppressWarnings(cov(log(output_diff[, 2:5]))/4)
cov_mat[4,] = 0; cov_mat[,4] = 0; 
cov_mat[4, 4] = 0.005
c1 = cov_mat

##Initialise from past runs
(p2 = colMeans(output_diff[,6:9]))
cov_mat = cov(log(output_diff[, 6:9]))/5
cov_mat[4,] = 0; cov_mat[,4] = 0; 
cov_mat[4, 4] = 0.05
c2 = cov_mat
cov_mat = list(c1, c2)

## Initialise data structure
obser = data.frame(cas = c(sort(us_american$Cas), sort(native_american$Cas)), 
               force = rep(c("us", "nat"), c(NROW(us_american), NROW(native_american))))

pars = c(p1, p2)
kern= exp_cdf
verbose=TRUE
N = 100; thin =1

set.seed(4)
o= mcmc(pars, cov_mat, obser, N, thin=thin, kern=kern, verbose=TRUE) 
saveRDS(o, file="output/combined_mcmc.RData")


## Plot the results
o1 = o[[1]]
setnicepar()
plot(o1[,1], type="l");

setnicepar(mfrow=c(2, 2))
plot(o1[,2], type="l");abline(h=mean(output_diff[,2]), col=2)
plot(o1[,6], type="l");abline(h=mean(output_diff[,6]), col=2)
plot(o1[,3], type="l");abline(h=mean(output_diff[,3]), col=2)
plot(o1[,7], type="l");abline(h=mean(output_diff[,7]), col=2)

setnicepar(mfrow=c(2, 2))
plot(o1[,4], type="l");abline(h=mean(output_diff[,4]), col=2)
plot(o1[,8], type="l");abline(h=mean(output_diff[,8]), col=2)
plot(o1[,5], type="l");abline(h=mean(output_diff[,5]), col=2)
plot(o1[,9], type="l"); abline(h=mean(output_diff[,9]), col=2)

setnicepar(mfrow=c(2, 2))
hist(o1[,2], breaks="fd");hist(o1[,6], breaks="fd")
hist(o1[,3], breaks="fd");hist(o1[,7], breaks="fd")

setnicepar(mfrow=c(2, 2))
hist(o1[,4], breaks="fd");hist(o1[,8], breaks="fd")
hist(o1[,5], breaks="fd");hist(o1[,9], breaks="fd")


#tmp = o

o1 = readRDS(file="output/combined_mcmc.RData")[[1]]
setnicepar(mfrow=c(2, 2))
plot(o1[,2], type="l");plot(o1[,6], type="l")
plot(o1[,3], type="l");plot(o1[,7], type="l");
colMeans(o1)
colMeans(tmp1)
tmp1 = tmp[[1]]
setnicepar(mfrow=c(2, 2))
plot(tmp1[,2], type="l");plot(tmp1[,6], type="l")
plot(tmp1[,3], type="l");plot(tmp1[,7], type="l");

x1 = seq(0.1, 3, length.out=1000)
plot(x1, dlnorm(x1, mu_1, sd(sigma_1)))
mu_1 = -1.4;  mu_2 = -1.2;  sigma_1 = 1;  sigma_2 = 1;  tau = 0.7
