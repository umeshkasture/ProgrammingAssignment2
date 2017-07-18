## MakeCached Matrix creates data structure "m" to cache an inverse of square matrix
## Default value of "m" is set to NULL for testing "get" and "set" functions are implemented for
## matrix data and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setInvr <- function(Invr)
        m <<- Invr
    getInvr <- function()
        m
    list(
        set = set,
        get = get,
        setInvr = setInvr,
        getInvr = getInvr
    )
}


## cacheSolve replaces solve(x) to provide inverse of square matrix
## Prior call to makeCacheMatix expected. Inverse computed if stored value found to be NULL (as initialized)
## Value of inverse stored on first computation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvr()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvr(m)
    m
}
