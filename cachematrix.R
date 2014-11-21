## Put comments here that give an overall description of what your
## functions do

## sets a variable to the function to be used for calculating
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Returns the cached inverse of the matrix if avail
## if not avail in cache, calculates the inverse and returns it

cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix() 
  
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
