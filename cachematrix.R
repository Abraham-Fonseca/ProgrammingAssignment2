makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize the inverse property
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y # Assign the new matrix to x in the parent environment
    inv <<- NULL # Reset the inverse property since the matrix has changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Return the inverse matrix if it is already cached
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get() # Get the matrix
  inv <- solve(data, ...) # Compute the inverse through matrix multiplication
  x$setinverse(inv) # Set the inverse to the object for future use
  
  inv # Return the computed inverse
}

