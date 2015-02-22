## provide a set of funcitons to initialize, get, set a matrix
## and to get and set its inverse. To save processing cycles, if the
## inverse already caculated, use the cached version.
makeCacheMatrix <- function(x = matrix()) {
  
  ## create the matrix and access functions
  ##initialize inverse matrix so clean each invocation
  ## will initiaize a 1x1 with value NA
  inverse_matrix <- matrix()
  ## set the matrix value
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(z) {
    inverse_matrix <<- solve(z)
  }
  
  getInverse <- function() {
    inverse_matrix
  }
  ## list() takes tag=value pairs as arguments
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## if the inverse already calculated, use the cache 
  ## and return directly form the function
  if(!is.na(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  ## otherwise ge tthe matrix and solve for its inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}