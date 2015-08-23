## Assignment 2

# makeCacheMatrix is the function which creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invmtx <- NULL
    set <- function(y) {
        x <<- y
        invmtx <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invmtx <<- inverse
    getInverse <- function() invmtx
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#cacheSolve function computes the inverse matrix returned by makeCacheMatrix function. 
#When the inverse has already been calculated, it is returned from the cache. 
cacheSolve <- function(x, ...) {
    invmtx <- x$getInverse()
    if (!is.null(invmtx)) {
        message("getting cached data")
        return(invmtx)
    }
    mat <- x$get()
    invmtx <- solve(mat, ...)
    x$setInverse(invmtx)
    invmtx
}
