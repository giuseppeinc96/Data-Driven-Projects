library(mlogit)
library(matrixStats) 


# Read the Survey Data
# Note: To run the model for every pizza, replace the file name below with the desired pizza names.
# Pizza names: Carbonara, Margherita, Margherita_extra, Ndjua, Double_pepperoni,
# Mushroom_truffle, Smokey, Pizza_love, Cheese, Maiale

Data_members= read.csv("Carbonara.csv", header = TRUE)
members_subset= Data_members[Data_members[, 2] == 1, ]
N= nrow(members_subset)

# Calculate Average WTPs for Peak and NonPeak
avg_wtps_peak <- mean(members_subset$Peak)
avg_wtps_non_peak <- mean(members_subset$NonPeak)

# Calculate Standard Deviations for Peak and NonPeak
sd_wtps_peak <- sd(members_subset$Peak)
sd_wtps_non_peak <- sd(members_subset$NonPeak)

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

# optimal peak periods prices
price_peak = 10.30
price_non_peak = 8.60

get_attractions <- function(x) {
  price_non_peak = x[1]
  price_peak = x[2]
  
  # Calculating attraction values separately for Peak and NonPeak
  attraction_non_peak = exp((avg_wtps_non_peak - price_non_peak) / mu_non_peak)
  attraction_peak = exp((avg_wtps_peak - price_peak) / mu_peak)
  
  return(list(attraction_non_peak = attraction_non_peak, attraction_peak = attraction_peak))
}

attraction_values_for_known_prices <- get_attractions(c(price_non_peak, price_peak))

# Print the calculated attraction values
print(attraction_values_for_known_prices)
