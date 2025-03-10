#maximization problem
library(lpSolve)

#objective function
obj.fun <- c(30, 40, 50)

#constraints
mat <- matrix(c(1,2,3, 1,2,0, 1,1,1),nrow=3, ncol=3, byrow= TRUE)

#direction
direction <- c("<=", "<=", "<=")


rhs < - c(150, 90, 120)

#solution
solution = lp("min",obj.fun,mat, direction, rhs )
print(solution)
print(solution$solution)
print(solution$objval)