[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEstaticCop** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFEstaticCop

Published in: Statistics of Financial Markets

Description: 'The contour diagrams of the Gumbel, Clayton and Normal copula are plotted in three coordinate plans. The fitted, empirical observations are added as red dots.'

Keywords: copula, distribution, normal, normal distribution, plot, graphical representation, density, clayton, gumbel

See also: SFEclaytonMC, SFEplotCop, SFEtCop, SFEVaRcopulaSIM2ptv, SFEArchCopDensity, BCS_ClaytonMC, SFEfrechet, SFEgaussCop, SFEresDens, SFEtMC, SFScontourgumbel, SFEgaussCop

Author: Ostap Okhrin

Submitted: Wed, September 14 2011 by Awdesch Melzer

Datafiles: Representative_Data.txt

Example: 'The Gumbel-, Clayton- and Normal Copula are based on normal margins.'
```

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("copula", "fGarch", "fBasics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
S = read.table("Representative_Data.txt", skip = 1)
sel.pairs = c(3, 4)
X = S[-1, ]
for (i in 1:length(S[1, ])) X[, i] = log(S[-1, i]/S[-length(S[, i]), i])
bi.scatter.countour.plots = function() {
    eps = X
    sigma.t = eps
    params = matrix(0, 4, dim(S)[2])
    for (i in 1:length(S[1, ])) {
        fit = garchFit(~garch(1, 1), data = X[, i], trace = F)
        params[i, ] = fit@fit$coef
        eps[, i] = fit@residuals/fit@sigma.t
    }
}
eps = X
means = mean(eps)
sds = sd(eps)
params.margins = list(list(mean = means[sel.pairs[1]], sd = sds[sel.pairs[1]]), 
    list(mean = means[sel.pairs[2]], sd = sds[sel.pairs[2]]))

tau = cor(eps[, sel.pairs], method = "kendall")[1, 2]
gumbel.tau2theta = function(tau) {
    1/(1 - tau)
}
clayton.tau2theta = function(tau) {
    2 * tau/(1 - tau)
}
normal.tau2theta = function(tau) {
    sin(tau * pi/2)
}
# the function specifies which multivariate distribution constructed from
# copula is used (based on normal margins)
renditen.copula.fun = function(copula.name, tau, params.margins) {
    # the parameter of the specified Archimedean copula are defined above, e.g.
    # gumbel.tau2theta
    gumbel.cop = gumbelCopula(gumbel.tau2theta(tau))
    clayton.cop = claytonCopula(clayton.tau2theta(tau))
    normal.cop = normalCopula(normal.tau2theta(tau))
    if (copula.name == "gumbel") 
        mvdc(gumbel.cop, c("norm", "norm"), params.margins) else if (copula.name == "clayton") 
        mvdc(clayton.cop, c("norm", "norm"), params.margins) else if (copula.name == "normal") 
        mvdc(normal.cop, c("norm", "norm"), params.margins)
}

# plot
xylim = 0.1  # it is advised to keep the xylim, in order that the contour lines can be recognized
layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE))
contour(renditen.copula.fun("gumbel", tau, params.margins), dmvdc, xlim = c(-xylim, 
    xylim), ylim = c(-xylim, xylim), lwd = 1.5)
points(eps[, sel.pairs[1]], eps[, sel.pairs[2]], col = rgb(1, 0, 0, 0.7), lwd = 0.01, 
    pch = 19)
contour(renditen.copula.fun("clayton", tau, params.margins), dmvdc, xlim = c(-xylim, 
    xylim), ylim = c(-xylim, xylim), lwd = 1.5)
points(eps[, sel.pairs[1]], eps[, sel.pairs[2]], col = rgb(1, 0, 0, 0.7), lwd = 0.01, 
    pch = 19)
contour(renditen.copula.fun("normal", tau, params.margins), dmvdc, xlim = c(-xylim, 
    xylim), ylim = c(-xylim, xylim), lwd = 1.5)
points(eps[, sel.pairs[1]], eps[, sel.pairs[2]], col = rgb(1, 0, 0, 0.7), lwd = 0.01, 
    pch = 19)

```

automatically created on 2018-05-28