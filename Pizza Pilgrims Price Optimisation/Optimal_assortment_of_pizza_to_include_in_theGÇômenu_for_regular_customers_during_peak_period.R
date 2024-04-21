# Number of products
N = 10

# Attractions for the products
v = c(2.063105, 2.021895, 1.409226, 1.228669, 1.258808, 2.389266, 1.706074, 1.608945, 1.313047, 0.978599)

# Attraction for the outside option
v0 = 1.1

# Prices for the products
p = c(10.30, 13.20, 13.70, 15.30, 14.50, 11.85, 12.35, 12.60, 14.00, 16.40)

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

print("The optimal assortment of pizza to include in the menu for regular customers during the peak period is: 2, 3, 4, 5, 9, 10")
print(paste0("The optimal expected revenue is:", round(myresults$objval, 4)))
