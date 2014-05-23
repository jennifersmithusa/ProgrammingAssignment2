
##The makeCacheMatrix function creates a new matrix-like object and caches it's inverse.
##To do this, the function produces objects "s" and "x" and saves them in
##the parent environment, as well as in the global env. as "y" and NULL,
#respectively, through the setm function. "s" has been set as NULL so
##the function only computes a new inverse matrix for objects
##which haven't been run through this function before.

##The makeCacheMatrix function then defines three other functions:
##getm, which retrieves the previously computed matrix, cached as "x";
##setinv, which solves for the inverse of a new matrix, "s", and caches it as "solve".
##getinv, which runs the function if the matrix-like object has not been cached before
##(i.e. is "s").

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        setm <- function(y) {
                x <<- y
                s <<- NULL
        }
        getm <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(setm = setm,
             getm = getm,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function computes the inverse of the matrix-like object
## returned by the makeCacheMatrix function. If the inverse has already been
## calculated (and the matrix is intact), then cacheSolve()
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                print("getting cached data")
                return(s)
        }
        data <- x$getmatrix()
        s <- solve(data, ...)
        x$setinverse(s)
        print("calculating new data")
        s
}


