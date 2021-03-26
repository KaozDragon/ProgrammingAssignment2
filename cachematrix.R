## These functions will work together to calculate the inverse of a matrix and
## store it in cache. That way we can save computational resources and not 
## recalculate inverse when calling the function without changing the input
## matrix.

## This function includes 4 functions that are the setters and getters. A user
## can define the input matrix and feed it to cacheSolve to solve its 
## inverse. By assigning the functions to a list of function names, user can 
## access and change the stored data within the enclosed environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function (newMatrix) {
                x <<- newMatrix
                inv <<- NULL
        }
        getMatrix <- function() return(x)
        setInverse <- function(newInverse) inv <<- newInverse
        getInverse <- function() return(inv)
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
        
}


## CasheSolve funtion checks to see if there's already a computed inverse in
## cache. If there is cached data, it skips recomputing the matrix and return
## the cache. If cache value is NULL, compute inverse of the new matrix and
## store that in cache and return the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$getMatrix())
        x$setInverse(inv)
        return(inv)
        
        ## Return a matrix that is the inverse of 'x'
}
