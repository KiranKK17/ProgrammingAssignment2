# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the cached inverse
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(newValue) {
    x <<- newValue
    # Clear the cached inverse when the matrix is updated
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the cached inverse
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  # Function to get the cached inverse
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(x, ...) {
  # Check if the cached inverse is available
  inverse <- x$getInverse()
  
  # If the cached inverse is available, return it
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  # If the cached inverse is not available, compute it
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}
