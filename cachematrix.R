## THe two functions together makeCacheMatrix and cacheSolve together creates a special object 
## that stores the inverse of a matrix. It will not recalculate the inverse if it already exists

## makeCacheMatrix This creates the object that saves the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## taking an object of the type cacheMatrix cacheSolve function provides the methods
## to calulate the inverse if it does not already exist.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
