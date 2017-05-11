## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## "Matrix" object which supports setting and getting matrix as long as 
## setting ind getting inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function

## Inversion of matrix returned by makeCacheMatrix function (see above). 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
