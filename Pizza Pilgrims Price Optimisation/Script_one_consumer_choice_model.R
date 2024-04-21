# Set working directory to the location of the data files
# setwd("/path/to/your/directory")


# Read the Survey Data
# Note: To run the model for every pizza, replace the file name below with the desired pizza names.
# Pizza names: Carbonara, Margherita, Margherita_extra, Ndjua, Double_pepperoni,
# Mushroom_truffle, Smokey, Pizza_love, Cheese, Maiale)

Data = read.csv("Maiale.csv", header = TRUE)
Data$customer <- NULL

# Row count
N = nrow(Data)

# Price for non-peak
# The total number of price combinations is 20*20=400
surplusNonPeak <- rep(0, N)
surplusPeak <- rep(0, N)
demandNonPeak <- rep(0, 400)
demandPeak <- rep(0, 400)
index = 1

for (basePrice in seq(from = 1, to = 20, by = 1)) {
  for (peakPrice in seq(from = 1, to = 20, by = 1)) {
    for (i in 1:N) {
      surplusNonPeak[i] = max(Data[i, c(3)] - basePrice)
      surplusPeak[i] = Data[i, 2] - peakPrice
    }
    demandNonPeak[index] = sum((surplusNonPeak > surplusPeak) * (surplusNonPeak >= 0))
    demandPeak[index] = sum((surplusPeak >= surplusNonPeak) * (surplusPeak >= 0))
    index = index + 1
  }
}

# Create a data table for regressions
newdata <- data.frame(matrix(nrow = 400, ncol = 5))
colnames(newdata) <- c("index", "basePrice", "peakPrice", "NonPeakDemand", "PeakDemand")
index = 1

for (basePrice in seq(from = 1, to = 20, by = 1)) {
  for (peakPrice in seq(from = 1, to = 20, by = 1)) {
    newdata[index, 1] = index
    newdata[index, 2] = basePrice
    newdata[index, 3] = peakPrice
    newdata[index, 4] = demandNonPeak[index]
    newdata[index, 5] = demandPeak[index]
    index = index + 1
  }
}

# Visualizing Revenue as a Function of Base and Peak Price
newdata$revenue <- newdata$basePrice * newdata$NonPeakDemand + newdata$peakPrice * newdata$PeakDemand
library(lattice)
wireframe(revenue ~ basePrice * peakPrice, data = newdata)

# stargazer
library(stargazer)

# Run Regressions:
# Regression for the dependent variable NonPeakDemand
fit2NonPeak <- lm(NonPeakDemand ~ basePrice + peakPrice, data = newdata)
summary(fit2NonPeak)
a1 = coef(fit2NonPeak)[1]
b11 = coef(fit2NonPeak)[2]
b12 = coef(fit2NonPeak)[3]

# Regression for the dependent variable PeakDemand
fit2Peak <- lm(PeakDemand ~ basePrice + peakPrice, data = newdata)
a2 = coef(fit2Peak)[1]
b21 = coef(fit2Peak)[2]
b22 = coef(fit2Peak)[3]
stargazer(fit2NonPeak, fit2Peak, type = "text")

# Finding optimal revenue by optimization
library("nloptr")
# Differentiated Prices
eval_f <- function(x) {
  basePrice = x[1]
  peakPrice = x[2]
  NonPeakDemand = max(0, a1 + b11 * basePrice + b12 * peakPrice)
  PeakDemand = max(0, a2 + b21 * basePrice + b22 * peakPrice)
  revenue = basePrice * NonPeakDemand + peakPrice * PeakDemand
  objfunction = -revenue
  return(objfunction)
}
eval_g_ineq <- function(x) {
  basePrice = x[1]
  peakPrice = x[2]
  NonPeakDemand = max(0, a1 + b11 * basePrice + b12 * peakPrice)
  PeakDemand = max(0, a2 + b21 * basePrice + b22 * peakPrice)
  constraint <- c(-NonPeakDemand, -PeakDemand, x[1] - x[2])
  return(constraint)
}
# initial values
x0 <- c(6, 15)
# lower and upper bounds of control
lb <- c(6, 6)
ub <- c(20, 20)
opts <- list("algorithm" = "NLOPT_LN_COBYLA",
             "xtol_rel" = 1.0e-9,
             "maxeval" = 1000)
result <- nloptr(x0 = x0, eval_f = eval_f, lb = lb, ub = ub,
                 eval_g_ineq = eval_g_ineq, opts = opts)
# print(result)
priceOpt <- result$solution
RevenueOpt <- -result$objective

# Base price
print(paste("Optimal Base Price:", priceOpt[1]))
# Peak price
print(paste("Optimal Peak Price:", priceOpt[2]))
# Optimal revenue
print(paste("Optimal Revenue:", RevenueOpt))
