# Caching Inverse of a Matrix

# The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
#
# Input:
#   x: A matrix
# Output:
#   A matrix with functions to get/set value and get/set inverse

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  # getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  # return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


# The function cacheSolve computes the inverse of a matrix, but if the inverse 
# has already been calculated before, the cached inverse is returned.
#
# Input:
#   x: A matrix
#   ...: Extra arguments
# Output:
#   The inverse of the matrix

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # return cached matrix inverse if it has been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}
