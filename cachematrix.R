## The two functions below calculate and store the inverse of a given matrix to
## avoid repeating costly computations.

## makeCacheMatrix creates a list of 4 functions that set and get a given matrix and its inverse.
## x is the matrix, while i is the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve uses the set and get functions defined in makeCacheMatrix to only calculate the
## inverse of a given matrix if it hasn't previously been calculated.

cacheSolve <- function(x, ...) {
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
