## Caching the inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object.
## x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve function computes the inverse of the original matrix created by makeCacheMatrix function. 
## If the inverse of that matrix already exits and the matrix is not changed (i.e. same as the original matrix) then this function will retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
