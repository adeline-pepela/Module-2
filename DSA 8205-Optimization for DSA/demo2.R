library(lpSolve)
#objective function
obj.fun <- c(0.12, 0.15)
print(obj.fun)

#constraints
mat <- matrix(c(60,60, 12,6, 10,30),nrow=3, ncol=2,byrow=TRUE)

#direction
direction <- c(">=",">=",">=")
#RHS
rhs <- c(300, 36, 90)

#solution
solution =lp("min",obj.fun, mat, direction, rhs)
print(solution)
print(solution$solution)
print(solution$objval)

