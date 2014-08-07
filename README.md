plmcmc
======
[![Build Status](https://travis-ci.org/csgillespie/plmcmc.png?branch=master)](https://travis-ci.org/csgillespie/plmcmc)

This is an R package that accompanys the paper XXX. 

Overview of the repository
--------------------------

 * The `pkg` directory contains the R package. 
 * The `examples` directory contains the R script used to produce the plots in XXX. In order to run the examples, you will have to create an `input` directory.
 * The `output` directory contains the MCMC output from the `examples` directory.


Installation
------------

The `plmcmc` package can be install from github using `devtools`
```r
install.packages("devtools")
library("devtools")
install_github("plmcmc", "csgillespie", subdir="pkg")
```

Note: Windows users have to first install [Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

Examples
-------------

All examples mentioned in the paper are located in the `examples` directory. See [examples/README](examples/README.md) file for details getting the data and running the examples.

Issues
-------

 * If you have any problems with the code, either create an issue or email me directly.

