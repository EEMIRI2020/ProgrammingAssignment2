## Caching the Inverse of a Matrix


## The makeCacheMatrix function creates a "list" containing a function to
## set the value of the matrix, get the value of the matrix
## set the value of the Inverse, get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function returns a matrix that is the inverse of 'x'
## It first checks to see if the Inverse has already been calculated.
## If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the data
## and sets the value of the Inverse in the cache via the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("skips the computation")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
