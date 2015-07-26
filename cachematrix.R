
## Below function (makeCacheMatrix) creates a special "matrix" object which can cache its inverse.In addition to this, it creates a list which will
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }


##  Below function computes the inverse of the "matrix" created by makeCacheMatrix, which is defined above. 
## Below function checks whether inverse has already been calculated. If, Yes, then it will retrieve the inverse from the cache else calculate the Inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setInverse(invrs)
  invrs
  
}
