## In numerical analysis it is often required to solve large linear equation systems.
## In many cases the systems need to be solved several times with different right hand
## sides, but without a change of the system matrix. In such cases the inverse of the 
## matrix is calculated once and stored in the cache for the other calculations. 
## This procedure saves a lot of computational time, as calculation of the inverse is 
## a computationally expensive operation.
##
## Below are two functions that are used to create a special object that stores an 
## invertible numeric matrix and caches its inverse.

## The following function 'makeCacheMatrix' gets an invertible matrix x 
## and returns a list of four functions:
##
## set: set the value of the matrix
## get: get the value of the matrix
## setInverse: set the value of the inverse matrix
## getInverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The following function 'cacheSolve' gets an input object created by 'makeChacheMatrix'.
## First the 'getInverse' function is called. If it's called the first time it will 
## return 'NULL', which means that the inverse is not yet available and needs to be 
## calculated. It is then stored in the cache using the 'setInverse' function call.
## In all upcoming calls of 'cacheSolve' the inverse is taken from the cache until
## somewhere in the script the 'set' functions changes the original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
