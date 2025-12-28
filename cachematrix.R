## makeCacheMatrix: closure-based matrix container with cached inverse.
## - set() updates the matrix and invalidates the cached inverse
## - get() returns the current matrix
## - setinverse()/getinverse() manage the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # cached inverse; NULL means "not yet computed"
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: returns inverse of x$get().
## Uses cached inverse when available; otherwise computes solve() and caches result.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting the inverse from cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}