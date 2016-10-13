
## This function creates a special "matrix=x" object
## that caches its inverse (matrix).
## (dutch:deze funcie creeert de inverse matrix )

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## comment 
## The function cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes caches and returns it
## (dutch: de cache-solve functie geeft de inverse matrix die is gemaakt
## met de makecachematrix funtie)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
