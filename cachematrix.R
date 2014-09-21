## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ##    Get square matrix to be inversed
      s <- NULL
      set <- function(y)      {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      sets <- function(solve) s <<- solve
      gets <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}      



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ##    Return a matrix that is the inverse of 'x' from cache or solve( )

      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      y <- solve(data)
      s <- round(data %*% y)
      x$setsolve (s)
      s
}
