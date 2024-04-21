# Read the Survey Data
# Note: To run the model for every pizza, replace the file name below with the desired pizza names.
# Pizza names: Carbonara, Margherita, Margherita_extra, Ndjua, Double_pepperoni,
# Mushroom_truffle, Smokey, Pizza_love, Cheese, Maiale

Data = read.csv("Asparagus_pizza.csv", header = TRUE)

N <- nrow(Data)
surplus <- rep(0, N)
demand <- rep(0, 19)  # Matching the number of price points

# Calculate demand for each price point
index <- 1
for (Price in seq(from = 1, to = 19, by = 1)) {
  for (i in 1:N) {
    surplus[i] <- Data[i, 2] - Price
  }
  demand[index] <- sum(surplus >= 0)
  index <- index + 1
}

# Create a data frame for analysis
newdata <- data.frame(index=1:19, Price=seq(from=1, to=19, by=1), Demand=demand)

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


# For this example, let's say the cost of the pizza is £4.50 per unit
cost_per_unit <- 4.50

# Let's also assume that the pizzeria has a limited resource of pizza (100 units) throughout the spring, 
# and each pizza uses 100 grams. Then the number of pizzas that can be made is 1000
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
  
  # Estimate demand based on the regression coefficients
  demand <- max(0, intercept + slope * price)
  
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

# Run the optimization problem with constraint
result <- nloptr(x0 = x0, eval_f = eval_f, eval_g_ineq = eval_g_ineq, lb = lb, ub = ub, opts = opts)

# View the results
print(result)
priceOpt <- result$solution
priceOpt <- format(round(priceOpt, 2), nsmall = 2)
print(paste("Optimal value of Decision Variable(s):", priceOpt))

# We will use the optimal price as the initial list price for the special pizza when the season starts. 
# Let's also assume that it will implement a discount of 20% for each month until the end of the season, 
# and any remaining units can be salvaged (sold) at price p4= 6.50 at the end of the season 

# Calculate demands
D1 <- 482.1 - 23.94 * 12.30
D2 <- 482.1 - 23.94 * 9.84
D3 <- 482.1 - 23.94 * 7.90

# Possible price levels
prices <- c(12.30, 9.84, 7.90)

# Weekly demand at the corresponding price levels
meandemand <- c(D1, D2, D3)

# Initial Inventory
totalinv = 1000

# Total number of Weeks in the Selling Horizon
totalmonths = 3

# Objective Function
obj.fun <- c(prices * meandemand, 6.50)

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


#The same logic has been applied over Venison_pizza.csv and Wild_mushrooms_pizza.

