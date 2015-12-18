
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("neldermead", "Bessel")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

CIRml = function(Params) {
    lData = Model$Data
    end = Model$n
    DataF = lData[1:end - 1]
    DataL = lData[2:end]
    a = Params[1]
    b = Params[2]
    sigma = Params[3]
    
    c = 2 * a/(sigma^2 * (1 - exp(-a * Model$delta)))
    u = c * exp(-a * Model$delta) * DataF
    v = c * DataL
    q = 2 * a * b/sigma^2 - 1
    z = 2 * sqrt(u * v)
    bf = besselI(z, q, TRUE)
    lnL = -(Model$n - 1) * log(c) - sum(-u - v + 0.5 * q * log(v/u) + log(bf) + 
        z)
    return(lnL)
}

# load data
data = read.table("yield_US3month9808.txt")

# Model
Model = NULL
Model$Data = data[1:2600, 1]/100
Model$delta = 1/252
Model$n = length(Model$Data)
end = Model$n

# Least square innitial estimation
x2 = Model$Data[1:(end - 1)]
x1 = Model$Data[2:end]

xbar_1 = mean(x1)
xbar_2 = mean(x2)

x3 = x1 - xbar_1
x4 = x2 - xbar_2

y1 = sum(x3 * x4)/length(x1)
y2 = sum(x4 * x4)/length(x1)

a = 252 * log(y1/y2)
gama = exp(a/252)
b = (xbar_1 - gama * xbar_2)/(gama - 1)

y3 = x1 - b * (gama - 1) - gama * x2
y4 = (b/(2 * a)) * (gama - 1)^2 + (gama/a) * (gama - 1) * x2

sig = sum(y3^2/y4)/length(x1)

a = -a
b = -b
sigma = sqrt(sig)

InitialParams = c(a, b, sigma)

# optimize the Likelihood function
options = optimset(method = "fminsearch", MaxIter = 300, MaxFunEvals = 300, Display = "iter", 
    TolFun = c(1e-04), TolX = c(1e-04))

yhat = fminsearch(CIRml, x0 = InitialParams, options)

Results = NULL
Results$Params = yhat$x
Results$Fval = -yhat$fval/Model$n
Results$Exitflag = yhat$Exitflag

a = yhat$x[1]
b = yhat$x[2]
sigma = yhat$x[3]

# Estimates
rbind(a, b, sigma)
print(paste("log-likelihood = ", Results$Fval))
