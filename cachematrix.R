## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a  special "Matrix", which a Matrix as input
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
##cacheSolve calculates the inverse matrix of the special "vector"
##created with the makeCacheMatrix. it first checks to see if the
##inverse matrix has already been calculated. If so, it gets the
##inverse matrix from the cache and skips the computation. else,
##it calculates the inverse matrix of the data and sets the value
##of the Matrix in the cache via  setmatrix function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
