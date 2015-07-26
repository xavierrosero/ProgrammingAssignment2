## A couple of functions used to create a special matrix and to calculate its inverse.
## They take advantage from cache techniques to solve the inverse of a certain matrix
## only if it has not been already solved previously. Otherwise, a result is taken
## from cache.


## "makeCacheMatrix" creates a list containing a function to both, set and get the
## value of the vector, and set and get the value of the inverse. The function used
## to calculate the inverse, is the well known solve(.).

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) inv <<- solve
  getInvMatrix <- function() inv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
} 
  
## "cacheSolve" calculates the inverse of the special "matrix" created with the above function.
## It checks if the inverse for a certain matrix has already been calculated. If so, it gets
## the result from the cache and skips the calculation. Otherwise, it calculates
## the inverse by using the known "solve" function and sets its result in the cache via
## the "setInvMatrix" function.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
