HousePrices <- read.csv("C:/Bayes Business School/Analytics Methods for Business/HousePrices.txt", sep="")
HousePrices.noID <- HousePrices[,-1]

HousePrices.noID$driveway.bin <- ifelse(HousePrices.noID$driveway == 'yes', 1, 0)
HousePrices.noID$recreation_bin <- ifelse(HousePrices.noID$recreation == 'yes', 1, 0)
HousePrices.noID$fullbase_bin <- ifelse(HousePrices.noID$fullbase == 'yes', 1, 0)
HousePrices.noID$gasheat_bin <- ifelse(HousePrices.noID$gasheat == 'yes', 1, 0)
HousePrices.noID$aircon_bin <- ifelse(HousePrices.noID$aircon == 'yes', 1, 0)
HousePrices.noID$prefer_bin <- ifelse(HousePrices.noID$prefer == 'yes', 1, 0)
HousePrices.noID$driveway <- NULL
HousePrices.noID$fullbase <- NULL
HousePrices.noID$recreation <- NULL
HousePrices.noID <- HousePrices.noID[, !(names(HousePrices.noID) %in% c("gasheat", "aircon", "prefer"))]
HousePrices.noID$log_price <- log(HousePrices.noID$price)
HousePrices.noID$price <- NULL

lm.fit <- lm(formula = log_price ~ lotsize + bathrooms + bedrooms + garage + driveway + fullbase + gasheat + aircon + 
               prefer, data = HousePrices.noID)

par(mfrow=c(2,2))
plot(lm.fit)
summary(lm.fit)

rm(confidence)
confint(lm.fit, level = 0.99)

cookD <- cooks.distance(lm.fit)
sort(cookD)
plot(cookD, pch = 16, cex = 1, main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/(length(HousePrices.noID$log_price) - length(coef(lm.fit)) - 1), col = "red") 
influential_obs <- which(cookD > 4/(length(HousePrices.noID$log_price) - length(coef(lm.fit)) - 1))
print(influential_obs)

