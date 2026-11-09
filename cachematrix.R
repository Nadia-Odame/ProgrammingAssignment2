
## These functions calculate and cache the inverse of a matrix.
## The inverse is stored so that repeated calculations can be avoided
## when the original matrix has not changed.

## Creates a special matrix object that stores the matrix and its
## calculated inverse using lexical scoping.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() {
                x
        }

        setsolve <- function(solve) {
                inv <<- solve
        }

        getsolve <- function() {
                inv
        }

        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Calculates the inverse of the matrix or retrieves it from the cache
## if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()

        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)

        inv
}
