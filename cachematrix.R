## This function creates a special "matrix" object that can cache
## its inverse.  The special "matrix" is a list which contains 
## the matrix as one item in the list.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has already
## been calculated (and the matrix has not changed), then 
## cacheSolve should retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)   ## Return a matrix that is the inverse of 'x'
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}