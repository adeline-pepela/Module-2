library(lpSolve)
#objective function
obj.fun <- c(0.06, 0.10, 0.12)

#constraints
mat <- matrix(c(1,1,1, -0.05,0.01,0.03, 0.5,-0.5,-0.5, -0.25,0.75,-0.25 ),nrow=4, ncol=3, byrow=TRUE)
print(mat)
#direction
direction <- c("<=", "<=", "<=", "<=")

#rhs
rhs<- c(450000, 0, 0, 0)

#solution
solution = lp("max", obj.fun, mat, direction, rhs)
print(solution)
print(solution$solution)
print(solution$objval)

