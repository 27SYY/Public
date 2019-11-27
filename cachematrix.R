## According to the requirement in the assignment, the two functions,
## "my_matrix" and "cacheSolve", can create a matrix and cache its
## inverse.

## By using the following orders can test the inverse matrix
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > my_matrix$get()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) i <<- solveMatrix
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This following function, "cacheSolve", returns a matrix that is 
## the inverse of 'x'.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i      
}