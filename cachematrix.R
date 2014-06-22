## Put comments here that give an overall description of what your
## functions do

# Function returns the inverse of a matrix and caches the results
# for future reference.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Setup a local variable to store inverse
  matrix_inv <- NULL
  
  # Define methods
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix_inv <<- solve
  getinverse <- function() matrix_inv
  
  # Return a list with each method
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Check the cache
  m <- x$getinverse()
  
  # If cache exists, fetch and return
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, get the current data, run solve on it and store
  # it for future use.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  # Return new inverse
  m
}
