## makeCacheMatrix creates a "special matrix object" (a list of functions)
## that stores:
##   1) a matrix x
##   2) a cached inverse of x (inverse)
##
## The cached inverse is cleared whenever the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
  
  # This will store the cached inverse; NULL means "not computed yet"
  inverse <- NULL
  
  # Replace the stored matrix with a new one and clear the cached inverse
  set_matrix <- function(temp_m) {
    x <<- temp_m          # <<- updates x in the enclosing environment (inside makeCacheMatrix)
    inverse <<- NULL      # invalidate cache since matrix changed
  }
  
  # Return the currently stored matrix
  get_matrix <- function() x
  
  # Store a precomputed inverse into the cache
  set_inverse <- function(inver) inverse <<- inver
  
  # Return the cached inverse (or NULL if not cached)
  get_inverse <- function() inverse
  
  # Return a list of functions (this behaves like an "object" with methods)
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the matrix stored in the special object.
## If the inverse has already been computed and cached, it returns the cached value.

cacheSolve <- function(x, ...) {
  
  # Try to retrieve cached inverse first
  inverse <- x$get_inverse()
  
  # If it exists, return it immediately
  if (!is.null(inverse)) {
    message("getting the inverse from cached data")
    return(inverse)
  }
  
  # Otherwise, compute inverse, cache it, and return it
  mat <- x$get_matrix()
  inverse <- solve(mat, ...)      # use ... in case solve() needs extra arguments
  x$set_inverse(inverse)
  inverse                         # last expression returned
}
