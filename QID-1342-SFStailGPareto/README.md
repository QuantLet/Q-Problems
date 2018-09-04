[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFStailGPareto** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFStailGPareto

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: Estimates the parameters of a Generalized Pareto Distribution for the negative log-returns of a portfolio (Bayer, BMW, Siemens) for the time period from 1992-01-01 to 2006-09-21 and produces a QQ-plot and PP-plot. Corresponds to exercise 16.5 in SFS.

Keywords: GEV, distribution, pareto, returns

See also: SFEgpdist, SFEtailGEV_pp, SFSevt2, SFSmeanExcessFun, SFSportfolio, SFStailGEV, SFStailport, SFSvar_block_max_backtesting, SFSvar_block_max_params, SFSvar_pot_backtesting, SFSvar_pot_params, SFSvarblockmaxbacktesting, SFSvarpotbacktesting

Author: Lasse Groth

Submitted: Sat, October 01 2011 by Awdesch Melzer

Datafiles: Bay9906_close_2kPoints.txt, Bmw9906_close_2kPoints.txt, Sie9906_close_2kPoints.txt

Input: 
- Please change working directory.

Output: 
- QQ-plot and PP-plot with Generalized Pareto Distribution.

Example: PP plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Pareto Distribution with parameter = 0.0768 globally estimated with POT method. QQ plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Pareto Distribution with a global parameter = 0.0768 estimated with POT method.
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


# load data
a = read.table("Bay9906_close_2kPoints.txt")
b = read.table("Bmw9906_close_2kPoints.txt")
c = read.table("Sie9906_close_2kPoints.txt")

d = a + b + c  # Create the portfolio
lg = dim(d)
x = log(d[-lg[1], ]) - log(d[-1, ])  # Negative log-returns

n = 100  # Number of extreme observations
GPD = fit.GPDb(x, nextremes = n, method = "ml", information = "observed")  # Fit the Generalized Pareto Distribution

t = (1:n)/(n + 1)
y1 = qGPD(t, GPD$par.ests[1], GPD$par.ests[2])  # Calculate quantiles for the Generalized Pareto Distribution
GPD.POT = sort(GPD$data) - GPD$threshold  # Peak Over Treshol values
y2 = pGPD(GPD.POT, GPD$par.ests[1], GPD$par.ests[2])  # Calcualte probabilities for the generalized Pareto distribution


# Plot the QQ plot
dev.new()
plot(GPD.POT, y1, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y1, y1, type = "l", col = "red", lwd = 2)
title("QQ plot, Generalized Pareto Distribution")

# Plot the PP plot
dev.new()
plot(y2, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y2, y2, type = "l", col = "red", lwd = 2)
title("PP plot, Generalized Pareto Distribution")
```

automatically created on 2018-09-04