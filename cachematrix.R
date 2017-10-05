## This file consists in two functions.
## The pourpose of these functions is to give an example of how we can save time and CPU's resources.
## Discerning when it comes to computing an instruction and when it is the case to avoid computing, because it would return 
## a value previously obtained, we should be able to minimize CPU and RAM consuming.


## makeCacheMatrix() returns a list of functions that will handle the contents of a matrix, passed as an argument to it by default.
## Recalling these functions, which will handle global variables, we will be able to keep track of the changes made to the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setInv = setinv, getInv = getinv)
}

## cacheSolve(<matrix>, ...) is responsible for determining whether or not the data in the matrix has changed.
## In the positive case, it will processes the inverse of the matrix passed as an argument to makeCacheMatrix,
## stores it in a global variable, and then returns it.
## In the negative case, it will returns the previous result of the inverse matrix, stored in that global variable.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
