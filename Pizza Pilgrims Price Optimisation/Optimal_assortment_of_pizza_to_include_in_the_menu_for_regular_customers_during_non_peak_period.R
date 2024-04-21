# Number of products
N = 10

# Attractions for the products
v = c(0.07, 0.10, 0.06, 0.08, 0.05, 0.10, 0.14, 0.13, 0.06, 0.06)

# Attraction for the outside option
v0 = 0.008

# Prices for the products
p = c(8.6, 11.40, 11.2, 12.7, 11.7, 10.1, 10.8, 10.8, 11.35, 13.5)

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
print("The optimal assortment of pizza to include in the menu for regular customers during non-peak period is: 4, 10")
print(paste0("The optimal expected revenue is:", round(myresults$objval, 4)))
