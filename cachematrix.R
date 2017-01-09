## This function creates a special matrix, that returns a list that contains
## the following functions: 1- set the value of the matrix,
## 2- get the value of the matrix, 3- set the value of the inverted matrix,
## and 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function whether the inverse of the matrix exists, if not,
## will caluclate the inverse and returns the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
