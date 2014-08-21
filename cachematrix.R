## Write a pair of functions that cache the inverse of a matrix.
## 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {     # input x will be a matrix
      m <- NULL                                 # m is initial inverse matrix, reset to NULL each
                                                # time makeCacheMatrix is called
      set <- function(y) {                      # set the value of the matrix
            x <<- y                             
            m <<- NULL
      }
      get <- function() x                       # get the value of the matrix
      setInverse <- function(solve) m <<- solve # set the value of the inverse matrix
      getInverse <- function() m                # get the value of the inverse matrix
      list(set = set, get = get,                # returns a list of available sub-functions          
           setInverse = setInverse,
           getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {                # input list of subfunctions from makeCacheMatix
      
      m <- x$getInverse()                       # get existing inverse (may not exist)
      if(!is.null(m)) {                         # if the inverse already exist print the message
            message("getting cached data")
            return(m)
      }
      data <- x$get()                           # get matrix from makeCacheMatrix
      m <- solve(data, ...)                     # Return a matrix that is the inverse of 'x'
      x$setInverse(m)                           # set the value of the inverse within the makeCacheMatrix
      m                                         # returns m
}