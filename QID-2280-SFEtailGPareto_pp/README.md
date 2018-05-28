[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEtailGPareto_pp** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFEtailGPareto_pp

Published in: Statistics of Financial Markets

Description: 'Estimates the parameters of a Generalized Pareto distribution for the negative log-returns of portfolio (Bayer, BMW, Siemens, VW)for the time period from 2000-01-01 to 2012-12-31 and produces a P-P plot.'

Keywords: asset, data visualization, dax, financial, graphical representation, plot, portfolio, pp-plot, returns, log-returns, stock-price, pareto, generalized-pareto-model

See also: SFEportfolio, SFEportlogreturns, SFEdenport, SFEclose, SFEtailGEV_pp, SFEtailGEV_qq, SFEtailGPareto_qq, SFEMeanExcessFun, SFEgpdist

Author: Awdesch Melzer

Submitted: Fri, November 29 2013 by Awdesch Melzer

Datafiles: BAYER_close_0012.dat, BMW_close_0012.dat, SIEMENS_close_0012.dat, VW_close_0012.dat

```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("POT")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
a = read.table("BAYER_close_0012.dat")
b = read.table("BMW_close_0012.dat")
c = read.table("SIEMENS_close_0012.dat")
e = read.table("VW_close_0012.dat")

# Portfolio
d = a + b + c + e

n1 = dim(d)[1]  # length of portfolio
x = log(d[1:n1 - 1, ]/d[2:n1, ])  # negative log-returns

gpd = fitgpd(x, quantile(x, 0.95), est = "mle")  # 
n = gpd$nat
thr = gpd$threshold
scale = gpd$param[1]
shape = gpd$param[2]
data = gpd$data
exc = gpd$exceed
t = (1:n)/(n + 1)
y1 = qgpd(t, scale = scale, shape = shape)

gpdt = sort(exc) - thr
y2 = pgpd(gpdt, scale = scale, shape = shape)

# plot
plot(y2, t, col = "blue", pch = 15, bg = "blue", xlab = "", ylab = "", main = "PP plot, Generalized Pareto Distribution")
lines(y2, y2, type = "l", col = "red", lwd = 2)
```

automatically created on 2018-05-28