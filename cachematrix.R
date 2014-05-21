##Bellow is a set of two functions that can cache an inverse of a matrix

## The first funciton creates a matrix, which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set  <- function(y) {
    x <<- y
    inv  <<- NULL 
  }
  get  <- function() x
  setinv  <- function(solve) inv <<- solve
  getinv  <- function() inv
  list(set = set, get = get, setinv=setinv, getinv=getinv)
}


## The second function creates an inverse of a given matrix. In case the inverse has already been computed, 
## the function will retrieve it from the cache

cacheSolve <- function(x, ...) {
  inv  <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data  <- x$get()
  inv  <- solve(data, ...)
  x$setinv(inv)
  inv
}


