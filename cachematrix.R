## makeCacheMatrix manages a matrix with a cached inverse
## cacheSolve calculates the inverse matrix or returns the cached inverse
## the purpose is to avoid costly recalculation as much as possible 

## manage an inversable square matrix with cached inverse
## arguments
## x    a square invertable matrix
## returns a list of functions to set and get the matrix and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL ## the cached inverse matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculate the inverse matrix or return the cached value
## parameters
## x    an invertable square matrix
## ...  additional arguments to be passed to or from methods
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
