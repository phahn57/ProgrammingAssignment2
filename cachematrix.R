## Both functions together can be used to create a matrix and 
## calculate the 'inverse' matrix

## makeCacheMatrix is a function itself containing four functions 
## for setting,getting a matrix and setting and getting the inverse
## matrix of the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse matrix of a given matrix
## if the inverse matrix for the given matrix already is calculated
## and stored into 'inv' then the value of inv is diplayed and
## for the sane of computation costs the matrixx is not calculated again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
  

