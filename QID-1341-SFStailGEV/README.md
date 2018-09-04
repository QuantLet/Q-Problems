[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFStailGEV** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFStailGEV

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: 'Fits a Generalized Extreme Value Distribution to the negative log-returns of a portfolio (Bayer, BMW, Siemens) for the time period from 1992-01-01 to 2006-09-21 and produces a QQ-plot and a PP-plot. Corresponds to exercise 16.5 in SFS.'

Keywords: GEV, pp-plot, qq-plot, returns

See also: SFEtailGEV_pp, SFSevt2, SFSheavytail, SFSportfolio, SFSportfolio, SFSportfolio, SFStailGPareto, SFStailport, SFSvar_block_max_params, SFSvar_pot_params, SFSvarblockmaxbacktesting, SFSvarpotbacktesting

Author: Lasse Groth

Submitted: Sat, October 01 2011 by Awdesch Melzer

Datafiles: Bay9906_close_2kPoints.txt, Bmw9906_close_2kPoints.txt, Sie9906_close_2kPoints.txt

Input: 
- Please change working directory.

Output: 
- 'QQ-plot and PP-plot with Generalized Extreme Value Distribution.'

Example: 'PP plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Extreme Value Distribution with a global parameter = 0.0498 estimated with block maxima method.

QQ plot of 100 tail values of daily log-returns of portfolio (Bayer, BMW, Siemens) from 1992-01-01 to 2006-09-01 against Generalized Extreme Value Distribution with a global parameter = 0.0498 estimated with block maxima method.'
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
x = log(d[-lg[1], ]) - log(d[-1, ])  # Negative log-return

# Determine the Block Maxima data
T = length(x)
n = 20
k = T/n
z = matrix(, , , )

for (j in 1:k) {
    r = x[((j - 1) * n + 1):(j * n)]
    z[j] = max(r)
}
w = sort(z)

GEV = fit.GEV(z)  # Fit the Generalized Extreme Value Distribution

K = GEV$par.ests[1]  # shape parameter
mu = GEV$par.ests[2]  # location parameter
sigma = GEV$par.ests[3]  # scale parameter

t = (1:k)/(k + 1)

y1 = qGEV(t, K, mu, sigma)
y2 = pGEV(w, K, mu, sigma)

# Plot the QQ plot
dev.new()
plot(w, y1, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y1, y1, type = "l", col = "red", lwd = 2)
title("QQ plot, Generalized Extreme Value Distribution")

# Plot the PP plot
dev.new()
plot(y2, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y2, y2, type = "l", col = "red", lwd = 2)
title("PP plot, Generalized Pareto Distribution")
```

automatically created on 2018-09-04