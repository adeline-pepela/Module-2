library(lpSolve)
#objective function
obj.fun <- c(300, 36,90)
print(obj.fun)

#constraints
mat <- matrix(c(60,12,10, 60,6,30),nrow=2, ncol=3,byrow=TRUE)
print(mat)

#direction
direction <- c("<=","<=")
#RHS
rhs <- c(0.12, 0.15)

#solution
solution =lp("max",obj.fun, mat, direction, rhs)
print(solution)
print(solution$solution)
print(solution$objval)