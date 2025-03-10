library(lpSolve)
#objective function
obj.fun <- c(40,30)
print(obj.fun)
  
#constraints
mat<- matrix(c(1,2, 1,1, 3,2),nrow=3, ncol =2, byrow =TRUE)
print(mat)

direction <- c("<=", "<=", "<=")
#RHS
rhs <- c(16, 9, 24)

#solution
solution <- lp("max",obj.fun, mat, direction, rhs)
print(solution)
print(solution$solution)
print(solution$objval)