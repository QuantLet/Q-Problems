
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
