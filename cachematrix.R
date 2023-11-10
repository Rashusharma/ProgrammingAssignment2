# Create a function that generates a list representing a matrix cache
makeCacheMatrix <- function(x = matrix()) {
  # Initialize an object to store the inverse matrix
  j <- NULL
  
  # Define a function to set the matrix in the cache
  set <- function(y) {
    x <<- y  # Use '<<-' to assign 'y' to the outer variable 'x'
    j <<- NULL  # Reset the inverse matrix when the main matrix is updated
  }
  
  # Define a function to get the matrix from the cache
  get <- function() x
  
  # Define a function to set the inverse matrix in the cache
  setInverse <- function(inverse) j <<- inverse  # Use '<<-' to assign 'inverse' to the outer variable 'j'
  
  # Define a function to get the inverse matrix from the cache
  getInverse <- function() j 
  
  # Return a list of functions for setting and getting the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Create a function that calculates the inverse of a matrix and caches the result
cacheSolve <- function(x, ...) {
  # Get the inverse matrix from the cache
  j <- x$getInverse()
  
  # If the inverse matrix is already cached, return it and print a message
  if (!is.null(j)) {
    message("Getting cached data")
    return(j)
  }
  
  # If the inverse matrix is not cached, calculate it
  mat <- x$get()
  j <- solve(mat, ...)
  
  # Cache the inverse matrix for future use
  x$setInverse(j)
  
  # Return the inverse matrix
  j
}
