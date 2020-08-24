## This script contains two functions that enables the caching of the inverse of a matrix 
## Caching uses lexical scoping to speed up calculations in complex scripts or large datasets
library(matlib) ##using the matlib library for the "inv()" function to invert a matrix

## The make cache matrix takes a non-singular matrix input from the user, and returns a list of
## four functions that get and set data by downstream code. The make cache function requires
## the Cachesolve function to populate it in order to function properly. The << operator
## is used here to assign a variable existing in the global/parent environment to another that
## is local to or defined witin a function. In the function below X is assigned to the global y 
## variable since the Y variable is not otherwise defined in the set(). 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cachesolve function is the one where the actual inverse of the matrix passed in 
##makecachematrix function is calculated. I use the inv() from the matlib library instead
## of the solve () to calculate the inverse of the matrix.
## This function essentially checks to see if a cached inverse is present and 
##if not passes the inverse to the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}
