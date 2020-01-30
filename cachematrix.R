## The functions in this file caches the inverse of a matrix
## (which is expensive to compute)
## so that repeated computation for the same matrix is avoided

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(
    set = set,
    get = get,
    set_inv = set_inv,
    get_inv = get_inv
  )
}


## Function to compute inverse of the special "matrix" returned by the
## makeCacheMatrix function if inverse has not already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
