## Our goal is to write functions that can calculate the inverse of a matrix once and cache the result in a global environment for ease of recall in the future.  The purpose of caching the result is to reduce the amount of time a code needs to run if our code requires multiple iterations.

## The makeCacheMatrix function is creating a new variable "i" which stores the value of the inverse of a matrix to be entered at a later time.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix and stores it to variable "i".

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
