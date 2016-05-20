## These two functions work together to create a cached copy of an inversed matrix.
## The inversion operation is not re-computed if it has been cached already.

## makeCacheMatrix takes a matrix and returns a list of functions:
## set to set/change the value of the stored matrix
## get to get the stored matrix
## setInv to set the value of the inverse matrix
## getInv to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invMatrix <<- inv
        getInv <- function() invMatrix
        list(set = set, get = get , setInv = setInv, getInv = getInv)
}


## Takes the list returned by makeCacheMatrix()
## Checks if the inverse is already in cache
##      If in cache: retrieves inverse from cache, returns the inverse
##      If not: compute inverse, return it

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInv()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data,...)
        x$setInv(invMatrix)
        invMatrix
}
