# Number of products
N = 10

# Attractions for the products
v = c(2.41, 4, 2.61, 2.40, 2.58, 3.79, 2.94, 2.73, 2.50, 2.23)

# Attraction for the outside option
v0 = 1.5

# Prices for the products
p = c(9.40, 11.8, 11.9, 12.55, 12.3, 11, 11.1, 11.3, 12.1, 13.4)

# Normalizing attractions so that their sum equals 1
vtotal = sum(v) + v0
v = v / vtotal

# First choice probabilities
a = v

# Constructing the transition matrix
b = matrix(0, N, N)

for (i in 1:N) {
  for (j in 1:N) {
    if (i != j) {
      b[i, j] = v[j] / (1 - v[i])
    }
  }
}

options(digits = 4)

# The LP

# Objective Function
obj.fun <- c(p, rep(0, N))

# Constraint Left-Hand-Side
constraint1lhs <- diag(1, N, N)
constraint2lhs <- diag(1, N, N) - t(b)  # t(b) is transpose of b
constr <- cbind(constraint1lhs, constraint2lhs)

# Constraint Right-Hand-Side
rhs <- a

# Constraint Directions
constr.dir <- c(rep("=", N))

# Solution:
myresults <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)
myresults$solution  # optimal decision variable values

myresults$objval  # optimal objective function value
optimalassortment = which(myresults$solution[1:N] > 0)

print(paste0("The optimal expected revenue is:", round(myresults$objval, 4)))
print("The optimal assortment of pizza to include in the menu for students during the peak period is: 2, 3, 4, 5, 8, 9, 10")
