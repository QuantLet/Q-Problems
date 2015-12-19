
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