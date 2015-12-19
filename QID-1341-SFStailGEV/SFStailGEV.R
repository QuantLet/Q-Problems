
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