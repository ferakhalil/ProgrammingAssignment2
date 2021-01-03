## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix is a function that creates a matrix, store it and can cache the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
    # local variable m 
    m <- NULL
    set <- function(y) {
        
        # Global variable x and m         
        x <<- y
        m <<- NULL
    }
    get <- function() x
    #inv of matrix x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

#cacheSolve is a function that will return the inverse of the makeCacheMatrix either by retrieving it (if it has been calculated) or computing it if it is not available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}



