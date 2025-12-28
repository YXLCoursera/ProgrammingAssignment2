makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set_matrix <- function(temp_m) {
    x <<- temp_m
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inver) inverse <<- inver
  get_inverse <- function() inverse
  
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting the inverse from cached data")
    return(inverse)
  }
  
  mat <- x$get_matrix()
  inverse <- solve(mat, ...)
  x$set_inverse(inverse)
  inverse
  
}
