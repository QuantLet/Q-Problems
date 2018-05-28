[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFESumm** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFESumm

Published in: Statistics of Financial Markets

Description: 'Computes the statistical features of DAX monthly returns.'

Keywords: kurtosis, returns, skewness, statistics, summary, time-series, volatility, dax, financial

Author: Ying Chen, Cindy Lamm

Submitted: Fri, June 13 2014 by Philipp Gschoepf

Output: Statistical features of DAX monthly return.

Example: For the data set of DAX monthly returns, monthly return's [minimum, maximum, mean, median, std. error]= [-0.24229, 0.16563, 0.0095835, 0.012022, 0.056231] and [Annual Volatility, Skewness, Kurtosis]= [0.19479, -0.69339, 5.4702] are shown.
```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("moments")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
x = read.table("dax.dat")
n = dim(x)[1]
r = log(x$V1[2:n]) - log(x$V1[1:n - 1])

sk = round(skewness(r), 4)
ku = round(kurtosis(r), 4)
annvol = round(sqrt(var(r) * 12), 4)

# result printout
Monthly_return = list(Minimum = round(min(r), 4), Maximum = round(max(r), 4), 
    Mean = round(mean(r), 4), Median = round(median(r), 4), Std.Error = round(sd(r), 
        4), Annual_Volatility = annvol, Skewness = sk, Kurtosis = ku)

cat("Monthly returns: n", paste(paste(names(Monthly_return), Monthly_return, sep = " = "), 
    "n"))

# plot
plot(r, main = "Monthly DAX Return 1979.1-2000.10", t = "l", col = "blue", xlab = "Time", 
    ylab = "DAX Return", lwd = 1.5)
abline(h = 0, col = "darkgreen", lwd = 1.5)

```

automatically created on 2018-05-28