## The aim of this function is to avoid duplications in already calculated values

## MakeCacheMatrix calculates a "vector" which is a list containing
## a function to:
## set inverse of a matrix
## get the value of the inversed matrix

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

## CacheSolve  calculates the inverse of the matrix if not already calculated

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