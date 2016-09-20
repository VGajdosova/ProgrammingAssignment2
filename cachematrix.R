
## Code Background: Caching the inverse of a matrix rather than computing the inverse each time again can be of great benefit (time-saving).
## The code below represents two functions that use a particular object for the storing of a matrix and caching it's inverse.


## Function "makeCacheMatrix" creates an object  -- matrix -- which can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Function "cacheSolve" computes the inverse of -- matrix -- created by 
## the above function "makeCacheMatrix".  In case that the inverse has already been calculated (bearing no changes to matrix occured),
## the function retrieves the inverse from the cache printing out "getting cached data" on the screen above the matrix.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}


