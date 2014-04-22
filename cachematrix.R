## Coursera "R Programming" - Peer Assignment
##
##   Matrix inversion is usually a costly computation and there may be some 
##   benefit to caching the inverse of a matrix rather than compute it 
##   repeatedly

makeCacheMatrix <- function(x = numeric()) {
  # Creates a "special function" (structure) to stores a matrix and its inverse
  #
  # Args:
  #   x: the matrix
  #
  # Returns:
  #   A list of functions for getting and setting the matrix and 
  #     the inverse matrix

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) m <<- inverse
  get.inverse <- function() m
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

cacheSolve <- function(x, ...) {
  # Returns the inverse of a matrix stored in a "makeCacheMatrix"
  #   function (structure) by calculating it or by getting the 
  #   cached value
  #
  # Args:
  #   x: the "makeCacheMatrix" special-function
  #
  # Returns:
  #   The inverse matrix
  
  m <- x$get.inverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
