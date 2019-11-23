## According to the requirement in the assignment, the two functions,
## "my_matrix" and "cacheSolve", can create a matrix and cache its
## inverse.

## By using the following orders can test the inverse matrix
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > my_matrix$get()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set,
             get = get,
             setmean = setmean,
             getmean = getmean)
}

## This following function, "cacheSolve", returns a matrix that is 
## the inverse of 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmean()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
