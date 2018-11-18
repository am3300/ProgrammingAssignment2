## Create an matrix/object to prepare for cache operations around finding the inverse of a matrix.
## Returens cached inverse matrix if it already exists or creates anew.

## Creates cachematrix structure to prepare for cache functions of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmatrix <- function(mean) m <<- mean
   getmatrix <- function() m
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## Searches for and returns existing value of 'solved' matrix or calculates anew otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatrix(m)
   m
   }

