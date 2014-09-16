## These functions optimise the repeated inversion of an unchanged matrix.

## makeCacheMatrix
## From the provided matrix, this function creates a list that includes the
## functions to get and set the matrix value itself and also to calculate its inverse.
## When the inverse is created, it is cached internally

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialise the internally cached inverse
  inv <- NULL
  
  ## Set the matix and set the inverse to NULL
  ## to show it has not been calculated
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <-function() {
    x
  }
  
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  getinv <- function() {
    inv
  }
  
  ## Return the list of named elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Return the inverse of the matrix.
## If the inverse has previously been calculated, then return it.
## If not, calculate and store it.

cacheSolve <- function(x, ...) {
  ## Get the cached inverse out of "special" matrix
  inv <- x$getinv()
  
  ## If this is not NULL, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return (inv)
  }

  ## If the cached invere is NULL,
  ## calculate the inverse and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
