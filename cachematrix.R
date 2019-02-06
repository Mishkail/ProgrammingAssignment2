
## add matrix into the cash
makeCacheMatrix <- function(x = matrix()) {
    if (dim(x)[1] != dim(x)[2]) {
        message('Warrning, initial matrix should be square for inversing')
    } 
    m <- NULL
    setm <- function(y) {
        x <<- y
        m <<- NULL
    }
    getm <- function() x # our initial data
    setsolve <- function(solve) m <<- solve # function of inverse
    getsolve <- function() m # function of inverse of our initial data
    list(setm = setm, getm = getm, 
         setsolve = setsolve,
         getsolve = getsolve)
}

# solve inverse of square matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    # if it has already solved, then return value from cache
    if (!is.null(m)) {
        message("getting cached data") 
        return(m)
    } # if m is null then we solve inversion of new matrix
    data <- x$getm()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

