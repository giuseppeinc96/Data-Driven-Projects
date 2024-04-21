library(mlogit)
library(matrixStats)

# Read the Survey Data
# Note: To run the model for every pizza, replace the file name below with the desired pizza names.
# Pizza names: Carbonara, Margherita, Margherita_extra, Ndjua, Double_pepperoni,
# Mushroom_truffle, Smokey, Pizza_love, Cheese, Maiale
Data_students = read.csv("Carbonara.csv", header = TRUE)
student_subset = Data_students[Data_students[, 2] == 2, ]
N = nrow(student_subset)

# Calculate Average WTPs for Peak and NonPeak
avg_wtps_peak <- mean(student_subset$Peak)
avg_wtps_non_peak <- mean(student_subset$NonPeak)

# Calculate Standard Deviations for Peak and NonPeak
sd_wtps_peak <- sd(student_subset$Peak)
sd_wtps_non_peak <- sd(student_subset$NonPeak)

# Calculate Variances for Peak and NonPeak
var_wtps_peak <- sd_wtps_peak^2
var_wtps_non_peak <- sd_wtps_non_peak^2

# Calculating the mean variance
avg_var_wtps <- mean(c(var_wtps_peak, var_wtps_non_peak))

# Display calculated values
cat("Average WTP - Peak:", avg_wtps_peak, "\n")
cat("Average WTP - NonPeak:", avg_wtps_non_peak, "\n")
cat("Variance - Peak:", var_wtps_peak, "\n")
cat("Variance - NonPeak:", var_wtps_non_peak, "\n")
cat("Average Variance:", avg_var_wtps, "\n")

mu_peak = sqrt(6 * var_wtps_peak) / pi
mu_non_peak = sqrt(6 * var_wtps_non_peak) / pi

eval_f <- function(x) {
  price_non_peak = x[1]
  price_peak = x[2]
  
  # Calculating attraction values separately for Peak and NonPeak
  attraction_non_peak = exp((avg_wtps_non_peak - price_non_peak) / mu_non_peak)
  attraction_peak = exp((avg_wtps_peak - price_peak) / mu_peak)
  
  # Original objective function calculation
  purchaseProbs = c(attraction_non_peak, attraction_peak) / (sum(c(attraction_non_peak, attraction_peak)) + 1)
  revenue = N * sum(c(price_non_peak, price_peak) * purchaseProbs)
  objfunction = -revenue  # Negative for maximization
  return(objfunction)
}

# Optimization setup remains the same
x0 <- c(4, 8)  # Initial guesses for non-peak and peak prices
lb <- c(2, 2)  # Lower bounds
ub <- c(15, 20) # Upper bounds

opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-6, "maxeval" = 1000)

get_attractions <- function(x) {
  price_non_peak = x[1]
  price_peak = x[2]
  
  # Calculating attraction values separately for Peak and NonPeak
  attraction_non_peak = exp((avg_wtps_non_peak - price_non_peak) / mu_non_peak)
  attraction_peak = exp((avg_wtps_peak - price_peak) / mu_peak)
  
  return(list(attraction_non_peak = attraction_non_peak, attraction_peak = attraction_peak))
}

# Running the optimization
result <- nloptr(x0 = x0, eval_f = eval_f, lb = lb, ub = ub, opts = opts)

# Extracting optimized prices
priceOpt <- round(result$solution, 2)
attraction_values <- get_attractions(priceOpt)
print(attraction_values)
print(paste("Optimal Non-Peak Price:", priceOpt[1]))
print(paste("Optimal Peak Price:", priceOpt[2]))
