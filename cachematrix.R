## This program consist of two main functions(makeCacheMatrix and cacheSolve) that are use to cache the inverse of the matrix

## this function consists 4 functions set(),get(),setinverse() and getinverse() that will be useful in creating and caching a matix
makeCachematrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse){ inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## This function will be useful in finding the inverse of the matrix created.

cacheSolve<- function(mat, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}
