## A set of functions that compute and cache the inversion matrix for an invertable matrix.  Built
## around the 'makeVector' and 'cachemean' functions provided by rdpeng.

## 'makeCacheMatrix' takes a matrix arguement and returns a list which includes the matrix and some 
## helper functions for caching the inversion calculation.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve' takes the container created with the 'makeCacheMatrix' function and attempts to 
## determine the inversion matrix.  If available, it will use a previously computed matrix stored
## in the container.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
