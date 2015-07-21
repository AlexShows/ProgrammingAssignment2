# Create a special matrix that operates on a 
# cached version of the data if the matrix
# has already been calculated, or creates
# the matrix if it has not been created and
# cached previously
makeCacheMatrix <- function(x = matrix()) {
  
  # Set the inverse as NULL the first
  # time setting up the cached matrix
  # but future invocations can pull the
  # cached matrix or cached inverse
  m <- NULL
  
  # Store the matrix local to this function
  # as x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Calling get on the cached matrix returns
  # the original matrix, stored persistently
  # in the above set function
  get <- function() x
  
  # Setting the inverse solves and saves 
  # the solution in a persistent way
  setinverse <- function(solve) m <<-solve
  
  # If the inverse has never been solved for,
  # this returns NULL, but if its inverse
  # has been found already, it will return
  # the cached version, saving the compute
  # time required for the inverse
  getinverse <- function() m
  
  # Return the list of functions defined 
  # above so that they can be called on
  # the cached matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# Solve for the inverse of a matrix, using a
# cached version if it is already present in
# the special matrix created previously, 
# saving computation time on future requests
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  # First, check to see if the inverse of the
  # matrix is computed and cached already
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the matrix has not been solved for
  # its inverse, we need to compute the 
  # inverse and cache it
  data <- x$get()
  # Solve for the inverse...
  m <- solve(data, ...)
  # ...then cache it...
  x$setinverse(m)
  # ...then return it.
  m
  
}
