## Function contining a list of functions to cache and retreive matrices and their inverse squares. Assumes the matrix
## being passed is invertible. 
## Usage: 
## >x <- matrix(c(2,1,1,2), nrow=2, ncol=2)
## >y <- makeCacheMatrix(x)
## >cacheSolve(y)
## [,1]       [,2]
## [1,]  0.6666667 -0.3333333
## [2,] -0.3333333  0.6666667
## > cacheSolve(y)
## getting cached data
## [,1]       [,2]
## [1,]  0.6666667 -0.3333333
## [2,] -0.3333333  0.6666667

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invX <<- solve
  getInverse <- function() invX
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## A function that will retrieve the cached inverse square of a matrix if it exists.
cacheSolve <- function(x, ...) {
  invX <- x$getInverse()
  ## Return the inverse square of matrix x, if it has been cached
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  ## Retreive the matrix
  data <- x$get()
  ## Calculate its inverse square
  invX <- solve(data, ...)
  ## Cache the inverse square and return it
  x$setInverse(invX)
  invX
}
