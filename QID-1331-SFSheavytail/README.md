[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFSheavytail** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFSheavytail

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: 'Simulation of 500 random normal (left) and 1.5-stable (right) normal variables with 25% and 75% quantiles (black lines and 2.5% and 97.5% quantiles (red lines) of the distributions. Refers to exercise 16.1 in SFS.'

Keywords: distribution, normal, quantile, random, stable

See also: SFSevt2, SFSmeanExcessFun, SFSmsr1, SFSportfolio, SFStailGEV, SFStailGEV, SFStailGPareto, SFStailport, SFSvar_block_max_backtesting, SFSvar_block_max_params, SFSvar_pot_backtesting, SFSvar_pot_params

Author: Lasse Groth

Submitted: Mon, September 26 2011 by Awdesch Melzer

Output: Plot of the simulated normal random variables with different quantiles.

Example: Simulation of 500 1.5-stable and normal variables.


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

n = 500

# Simulate random normal variables and stable normal variable.
nor = rnorm(n, 0, 1)
sta = rstable(n, 1.5, 0)

# Determine the quantiles
sumnor = quantile(nor, c(0.025, 0.25, 0.5, 0.75, 0.975))
noru = sumnor[2] * matrix(1, n, 1)
norl = sumnor[4] * matrix(1, n, 1)
noruu = sumnor[1] * matrix(1, n, 1)
norll = sumnor[5] * matrix(1, n, 1)

sumsta = quantile(sta, c(0.025, 0.25, 0.5, 0.75, 0.975))
stau = sumsta[2] * matrix(1, n, 1)
stal = sumsta[4] * matrix(1, n, 1)
stauu = sumsta[1] * matrix(1, n, 1)
stall = sumsta[5] * matrix(1, n, 1)

# Plot the random variables
par(mfrow = c(1, 2))
plot(nor, col = "Blue", xlim = c(0, n), ylim = c(-10, 10), xlab = "", ylab = "")
title("Normal random variables")

lines(noru, col = "black", lwd = 3)
lines(norl, col = "black", lwd = 3)
lines(noruu, col = "red", lwd = 3)
lines(norll, col = "red", lwd = 3)


plot(sta, col = "Blue", xlim = c(0, n), ylim = c(-10, 10), xlab = "", ylab = "")
title("1.5 stable random variables")

lines(stau, col = "black", lwd = 3)
lines(stal, col = "black", lwd = 3)
lines(stauu, col = "red", lwd = 3)
lines(stall, col = "red", lwd = 3)
```

automatically created on 2018-05-28