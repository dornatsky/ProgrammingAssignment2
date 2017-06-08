## Creates a matrix wrapper, which, in addition to storing the data itself, 
## contains functions allowing the caller to calculate an inverse matrix 
## and memoize the result.
## The interface will be later used by cacheSolve to retrieve the calculated 
## value.
## Use of camel case is deliberate, convention used in the sample is hard to read.

makeCacheMatrix <- function(x = matrix()) {
  memoizedInverse <- NULL
  
  # I got rid of the set function since the wrapping closure does the job just fine
  get <- function() x
  setInverse <- function(inverse) memoizedInverse <<- inverse
  getInverse <- function() memoizedInverse
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Actually computes the the inverse, using the memoized solution, if present
# Tested with diagonal matrices and
# m <- matrix (c(4, 2, 7, 6), 2, 2)
# x<-makeCacheMatrix(diag(3))
# cacheSolve(x)
cacheSolve <- function(x, ...) {
  memoizedInverse <- x$getInverse()
  if(!is.null(memoizedInverse)) {
    message("getting cached data")
    return(memoizedInverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}