[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFSevt2** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFSevt2

Published in: 'Statistics of Financial Markets : Exercises and Solutions'

Description: 'Generates CDFs for normal distribution of a pseudo random variable with extreme value (Gumbel, Frechet and Weibull) and a random normal distributed variable.'

Keywords: Frechet, Weibull, cdf, distribution, extreme-value, gumbel, normal, pp-plot, random

See also: SFEevt1, SFEevt2, SFSheavytail, SFSmeanExcessFun, SFSmeffrechet, SFSmsr1, SFSportfolio, SFSportfolio, SFSportfolio, SFStailGEV, SFStailGEV, SFStailGPareto, SFStailGPareto, SFStailport, SFSvar_block_max_backtesting, SFSvar_block_max_params, SFSvar_pot_backtesting, SFSvar_pot_params

Author: Lasse Groth

Submitted: Mon, September 26 2011 by Awdesch Melzer

Example: 
- PP plot, Gumbel
- PP plot, Frechet
- PP plot, Weibull.
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

n = 100

# Gumbel
gumb1 = rGEV(100, xi = 0, mu = 0, sigma = 1)
gumb2 = sort(gumb1)
gumb = pnorm(gumb2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(gumb, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Gumbel")

# Frechet
frec1 = rGEV(100, xi = 0.5, sigma = 0.5, mu = 1)
frec2 = sort(frec1)
frec = pnorm(frec2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(frec, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""), 
    xlim = c(0, 1), ylim = c(0, 1))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Frechet")

# Weibull
weib1 = rGEV(100, xi = -0.5, sigma = 0.5, mu = -1)
weib2 = sort(weib1)
weib = pnorm(weib2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(weib, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""), 
    xlim = c(0, 1), ylim = c(0, 1))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Weibull")
```

automatically created on 2018-05-28