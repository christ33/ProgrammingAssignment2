## Matrix inversion and Caching the Inverse of a Matrix


## This function creates a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() return(x)
        setinv <- function(inv) inverse <<- inv
        getinv <- function() return(inverse)
        return(list(set = set, get = get,
             setinv = setinv,
             getinv = getinv))
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## Computing the inverse of a square matrix can be done with the solve function in R. 
        ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
