###############################################################################
#              Matrix inversion is usually a costly computation               #
#            and there may be some benefit to caching the inverse             #
#               of a matrix rather than compute it repeatedly.                #
#     The following functions calculate and cache the inverse of a matrix.    #
###############################################################################

# The following function makeCacheMatrix gets a matrix x as input and 
# creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function cacheSolve gets as input the output of
# makeCacheMatrix and returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the inverse from the cache and skips the
# computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value 
# of the inverse in the cache via the setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Usage example
#
# > myMatrix <- matrix( c(2,0,2,0,1,2,0,0,2), ncol=3, byrow=TRUE )
# > myCache  <- makeCacheMatrix( myMatrix )
#      [,1] [,2] [,3]
# [1,]    2    0    2
# [2,]    0    1    2
# [3,]    0    0    2
#
# First run, the matrix is not cached yet.
# > cacheSolve( myCache )
#      [,1] [,2] [,3]
# [1,]  0.5    0 -0.5
# [2,]  0.0    1 -1.0
# [3,]  0.0    0  0.5
#
# Second run on the same matrix
# > cacheSolve( myCache )
# getting cached data
#      [,1] [,2] [,3]
# [1,]  0.5    0 -0.5
# [2,]  0.0    1 -1.0
# [3,]  0.0    0  0.5
#