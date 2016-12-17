## Data Science Tools -- Week 3 Project
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly.

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
## set: to set the value of the matrix
## get: to get the value of the matrix
## setreverse: to set the value of inverse of the matrix
## getreverse: to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
                x <<- y
                m <<- NULL
             }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## It checks if the inverse has already been 
## calculated (and the matrix has not changed). 
## If yes, then cacheSolve should retrieve the inverse matrix from the cache.
## If no, cacheSolve computes the inverse matrix.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
           message("getting cached data")
           return(m)
       }

       data <- x$get()
       m <- solve(data)
       x$setinverse(m)
       m
}
