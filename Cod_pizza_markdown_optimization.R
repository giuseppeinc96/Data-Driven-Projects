library(lpSolve)


# Read the Survey Data
# Note: To run the model for every pizza, replace the file name below with the desired pizza names.
# Pizza names: Carbonara, Margherita, Margherita_extra, Ndjua, Double_pepperoni,
# Mushroom_truffle, Smokey, Pizza_love, Cheese, Maiale

Data = read.csv("CodPizza.csv", header = TRUE)

# Assuming Data is loaded correctly
N <- nrow(Data)
surplus <- rep(0, N)
demand <- rep(0, 30)  # Matching the number of price points

# Calculate demand for each price point
index <- 1
for (Price in seq(from = 1, to = 30, by = 1)) {
  for (i in 1:N) {
    surplus[i] <- Data[i, 2] - Price  # Assuming the second column of Data contains WTP
  }
  demand[index] <- sum(surplus >= 0)
  index <- index + 1
}

# Create a data frame for analysis
newdata <- data.frame(index=1:30, Price=seq(from=1, to=30, by=1), Demand=demand)

# Calculate revenue
newdata$revenue <- newdata$Price * newdata$Demand

# Run linear regression
model <- lm(Demand ~ Price, data=newdata)

# Summary of the model to see the coefficients and statistics
summary(model)

intercept <- model$coefficients[1]
slope <- model$coefficients[2]
print(intercept)
print(slope)


# For this example, let's say the cost of the pizza is $5 per unit
cost_per_unit <- 6

# Let's also assume that the pizzeria has limited resources of pizza (100 units) throughout the summer, 
# and each pizza uses 1 unit. Then the number of pizzas that can be made is 1000
cap <- 1000

# Define the objective function for profit maximization
eval_f <- function(x) {
  price <- x[1]
  # Estimate demand based on the number of WTP values greater than or equal to the price
  demand <- max(0, intercept + slope * price)
  # Calculate profit
  profit <- (price - cost_per_unit) * demand
  # Since we are using a minimizer, we take the negative of profit to maximize profit
  objfunction <- -profit
  return(objfunction)
}

eval_g_ineq <- function(x) {
  price <- x[1]
  # Estimate demand based on the number of WTP values greater than or equal to the price
  demand <- max(0, intercept + slope * price)
  # Calculate profit
  profit <- (price - cost_per_unit) * demand
  
  # Let's add this constraint
  constraint <- c(demand - cap)
  return(constraint)
}

# Set initial values for decision variables (starting guess for price)
x0 <- c(10)

# Set lower and upper bounds for decision variables
lb <- c(5)
ub <- c(25)

# Set optimization options
opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-6, "maxeval" = 1000)

# Run the optimization problem
# Run the optimization problem with constraint
result <- nloptr(x0 = x0, eval_f = eval_f, eval_g_ineq = eval_g_ineq, lb = lb, ub = ub, opts = opts)

# View the results
print(result)
priceOpt <- result$solution
priceOpt <- format(round(priceOpt, 2), nsmall = 2)
print(paste("Optimal value of Decision Variable(s):", priceOpt))

# We will use the optimal price as the initial list price for the special pizza when the season starts. 
# Let's also assume that it will implement a discount of 20% for each month until the end of the season, 
# and any remaining units can be salvaged (sold) at price p4 = 8.25 at the end of the season 

# Our The price-response function is the following: 517.6 - 19.56p
# Calculate demands
D4 <- 517.6 - 19.56 * 16.20
D2 <- 517.6 - 19.56 * 13.05
D3 <- 517.6 - 19.56 * 10.40
D4
# Possible price levels
prices <- c(16.30, 13.05, 10.40)

# Weekly demand at the corresponding price levels
meandemand <- c(201, 262, 314)

# Initial Inventory
totalinv = 1000

# Total number of Weeks in the Selling Horizon
totalmonths = 3

# Objective Function
obj.fun <- c(prices * meandemand, 8.25)

# Coefficients for Left-Hand-Side of the Constraint Matrix
unitssold <- c(meandemand, 1)
monthss <- c(1, 1, 1, 0)
firstmonthprice <- c(1, 0, 0, 0)
constr <- rbind(unitssold, monthss, firstmonthprice)

# Right-Hand-Side values for the constraints
rhs <- c(totalinv, totalmonths, 1)

# Constraint Directions
constr.dir <- c("=", "<=", ">=")

# Running the LP optimization Problem
markdownopt <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)

# The optimal solution
print(paste("The optimal solution is: ", markdownopt$solution))

# The optimal revenue
print(paste("The optimal revenue is: ", markdownopt$objval))
