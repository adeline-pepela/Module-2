library(lpSolve)
#Question1
#objection function
obj.fun <- c(6, 3)
#constraints

mat<- matrix (c(2,4, 4,3), nrow=2, ncol=2, byrow = TRUE)

#direction

direction <- c(">=",">=")

#rhs
rhs <- c(16, 24)

solution <- lp("min",obj.fun, mat, direction, rhs )
print(solution)
print(solution$solution)
print(solution$objval)


#part2
#objective function
obj.fun <- c(600, 500)

#constraint
mat = matrix (c(2,1, 1,2),nrow=2, ncol=2, byrow=TRUE)

#direction
direction = c(">=", ">=")

#rhs
rhs <- c(80, 60)

#solution
solution <- lp("min",obj.fun, mat, direction, rhs)
print(solution) #26666.67
print(solution$solution) #x1= 33.33333, x2= 13.33333
print(solution$objval)

#Question2

#maximization 
#objective function
obj.fun <- c(3, 5, 4)

#constraint
mat <- matrix (c(2,3,0, 0,2,5, 3,2,4 ),nrow=3, ncol= 3, byrow=TRUE)

#direction
direction = c("<=", "<=", "<=")

#rhs
rhs <- c(8, 10, 15)

#solution
solution <- lp("max", obj.fun, mat, direction, rhs)
print(solution) #18.65854
print(solution$solution) #2.1707 1.2195 1.51219 
print(solution$objval) 