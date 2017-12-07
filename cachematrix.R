## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. This file contains a pair of functions 
## that cache the inverse of a matrix.
##
## Example of usage:
##
##> m <- matrix(c(2, 2, 3, 2), 2, 2)
##> x <- makeCacheMatrix(m)
##> inv <- cacheSolve(x)
##> if (!identical(inv, solve(m)))
##> {
##>   stop("cacheSolve function returns a different value than it's non-cached variation")
##> }
##> 
##> # use the cache again
##> inv <- cacheSolve(x)
##> # Should be printing "getting cached data"
##> 
##> # Trying but with a different matrix
##> m2 <- matrix(c(1, -1, 1, 2), 2, 2)
##> x$set(m2)
##> inv2 <- cacheSolve(x)
##> # this time the cache has been recomputed
##> if (!identical(inv2, solve(m2)))
##> {
##>   stop("cacheSolve function returns a different value than it's non-cached variation")
##> }
##> # use the cache again
##> inv <- cacheSolve(x)
##> # Should be printing "getting cached data"
##
##
## For a better understanding of the '<<-' operator, you can read
## 
##  http://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r



## This function create a "special" matrix object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(x) inverse <<- x
  getinverse <- function() inverse
  
  list(setinverse = setinverse, getinverse = getinverse, set = set, get = get)
}

## This function will return the inverse of the given matrix 
## object previously created by "makeCacheMatrix" (using 
## 'solve'). If the inverse was already computed for that
## matrix object instance, it will simply used a cached value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}


# A <- matrix( c(5, 1, 0,
#                3,-1, 2,
#                4, 0,-1), nrow=3, byrow=TRUE)
# 
# sA = makeCacheMatrix(A)
# cacheSolve(sA)
# 
# B <- matrix( c(4, 2, 2,
#                2, 3, 1,
#                2, 1, 3), nrow=3, byrow=TRUE)
# 
# sB = makeCacheMatrix(B)
# cacheSolve(sB)
