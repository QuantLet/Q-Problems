[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFSmeanExcessFun** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFSmeanExcessFun

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: 'Plots the empirical mean excess function, the mean excess function of generalized Pareto distribution, the mean excess function of Pareto distribution with parameter estimated with Hill estimator for the negative log-returns of portfolio (Bayer, BMW, Siemens), time period: from 1992-01-01 to 2006-09-21. Refers to exercise 16.8 in SFS.'

Keywords: MEF, hill-estimator, pareto, returns

See also: SFSevt2, SFSheavytail, SFSmsr1, SFSportfolio, SFStailGPareto, SFStailport, SFSvar_block_max_backtesting, SFSvar_block_max_params, SFSvar_pot_backtesting, SFSvar_pot_params

Author: Lasse Groth

Submitted: Wed, December 07 2011 by Dedy Dwi Prastyo

Datafiles: Bay9906_close_2kPoints.txt, Bmw9906_close_2kPoints.txt, Sie9906_close_2kPoints.txt

Output: 
- Plot of the empirical mean excess function, the mean excess function of generalized Pareto distribution, the mean excess function of Pareto distribution with parameter estiamted with Hill estimator.

Example: Empirical mean excess plot (blue line), mean excess plot of generalized Pareto distribution (black line) and mean excess plot of Pareto distribution with parameter estimated with Hill estimator (red line) for portfolio (Bayer, BMW, Siemens) negative log-returns from 1992-01-01 to 2006-09-01.
```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("matlab", "QRMlib")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# load data
a = read.table("Bay9906_close_2kPoints.txt")
b = read.table("Bmw9906_close_2kPoints.txt")
c = read.table("Sie9906_close_2kPoints.txt")

d = a + b + c  # Create the portfolio
lg = dim(d)
x = log(d[-lg[1], ]) - log(d[-1, ])  # Negative log-returns

n = length(x)
x = sort(x, decreasing = TRUE)

m = 100
x1 = x[1:m]

# empirical mean excess function
t = x[1:(m + 1)]  # t must be >0

MEF = matrix(, , , )

for (i in 1:length(t)) {
    y = x[find(x > t[i])]
    MEF[i] = mean(y - t[i])
}

plot(t, MEF, type = "l", col = "blue", lwd = 3, ylim = c(0.005, 0.04), xlab = "u", 
    ylab = "e(u)")
title("Mean Excess Functions")

# mean excess function of generalized Pareto distribution, theorem 18.8
k = 100
GPD = fit.GPDb(x, nextremes = k, method = "ml", information = "observed")
K = GPD$par.ests[1]
sigma = GPD$par.ests[2]
gpme = (sigma + K * (t - mean(t)))/(1 - K)

lines(t, gpme, lwd = 3)

# Hill estimator, mean excess function of Pareto distribution
alphaH = (mean(log(x1)) - log(x1[k]))^(-1)
sigmaH = x1[k] * (k/n)^(1/alphaH)
gp1me = t/(alphaH - 1)

lines(t, gp1me, col = "red", lwd = 3, lty = 5)
```

automatically created on 2018-09-04