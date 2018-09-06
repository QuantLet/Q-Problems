[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEFreqFilt_new** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFEFreqFilt_new

Published in: Statistics of Financial Markets

Description: 'Applies first differencing and 12-month centered moving average filter to SOI data and plots the results in time and frequency representation.'

Keywords: 'Spectrum, Differencing, Financial Markets, moving average filter'

Author: 'Dedy D. Prastyo, Elisabeth Bommes'

Submitted: 'Thu, June 05 2014 by Sergey Nasekin'

Datafiles: 

Output: 'Spectrum and time series line plots of filtered SOI data.'

```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

soi = scan("http://www.stat.pitt.edu/stoffer/tsa2/data/soi.dat")

# Time domain
par(mfrow = c(1, 2))
plot.ts(diff(soi), col = "blue", lwd = 3, ylab = expression(S[t]))  # plot 1st difference of soi data
k = kernel("modified.daniell", 6)  # 12 month filter
MA12 = kernapply(soi, k)
plot.ts(MA12, col = "magenta", lwd = 3, ylab = expression(S[t]))

# Frequency domain
spectrum(soi, log = "no", main = "", col = "magenta", lwd = 3)
abline(v = 1/12, lty = "dotted")
abline(v = 1/48, lty = "dotted")
spectrum(MA12, spans = 9, log = "no", main = "", col = "blue", lwd = 3)
abline(v = 1/52, lty = "dotted")
```

automatically created on 2018-09-06