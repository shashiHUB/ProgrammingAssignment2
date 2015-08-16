## Main function

makeCacheMatrix <- function(x = matrix()) { 
  invMat = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invMat <<- NULL
  }
  get = function() x
  setinv = function(inverse) invMat <<- inverse 
  getinv = function() invMat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function cacheSolve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## check if exists in cache
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
    
  data <- x$get()
  ## Inverse the Matrix
  inv <- solve(data)
  # Call for Cache
  x$setinv(inv)
  inv
}


## This is only for understanding the inverse matrix
## https://www.khanacademy.org/math/precalculus/precalc-matrices/inverting_matrices/v/inverse-of-a-2x2-matrix
## cacheSolve <- function(x, ...) {
##    inv = solve(x)
##    inv
## }
##> x <-  c(3,-1,5,2)
##> dim(x) <- c(2,2)
##> x
## [,1] [,2]
## [1,]    3    5
## [2,]   -1    2

## Answer
## source("CacheMatrix.R")
##> cacheSolve(x)
## [,1]       [,2]
## [1,] 0.18181818 -0.4545455
## [2,] 0.09090909  0.2727273

## Final Tests for the original problem.
## > y <-  c(3,-1,5,2)
## > dim(y) <- c(2,2)
## > source("CacheMatrix.R")

## > a <- makeCacheMatrix() 
## > a$set(y)
## > cacheSolve(a)
