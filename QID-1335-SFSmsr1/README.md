[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFSmsr1_new** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFSmsr1_new

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: Shows the rate of convergence to infinity for the stable distributed random variables is higher than for standard normal variables. Plots the convergence rate of maximum for n random variables with a standard normal cdf and with a 1.1-stable cdf. Refers to exercise 16.2 in SFS.

Keywords: cdf, normal, random, stable

See also: SFSevt2, SFSheavytail, SFSmeanExcessFun, SFSportfolio, SFStailGEV, SFStailGPareto, SFStailport, SFSvar_block_max_params, SFSvar_pot_params

Author: Lasse Groth

Submitted: Fri, September 30 2011 by Awdesch Melzer

Output: 
- Plots of the convergence rate of maxima for standard normal cdf and 1.1-stable cdf.

Example: 'Convergence rate of maxima for standard normal cdf. Convergence rate of maximum for n random variables with a 1:1-stable cdf.'
```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("QRMlib")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


n0 = 10
i = n0
n = 10000/n0
flag = 1

m = n/n0

dat1 = dat2 = matrix(, 100, 2)

for (i in 1:m) {
    
    dat1[i, 1] = i * 10
    dat1[i, 2] = max(rnorm(i, mean = 0, sd = 1))  # for normal distributions
    
    dat2[i, 1] = i * 10
    dat2[i, 2] = max(rstable(i, 1.1, 0))  # for stable distributions
}

plot(dat1[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for normal cdf")

dev.new()
plot(dat2[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for stable cdf")
```

automatically created on 2018-09-04