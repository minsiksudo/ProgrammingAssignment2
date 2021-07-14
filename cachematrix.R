## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
##As the sample fuction, the makeCacheMatix was made

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){# Checking weather the inverse was already calculated.
    message("getting cached data") # if there is, just loading the inv from the x
    return(inv)
  }
  data <- x$get()#if inv is null, the dtat should be called
  inv <- solve(data) # and inverse is calculated using solve function
  x$setInverse(inv)
  inv      
}
