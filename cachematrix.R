## These two functions enable a user to create a matrix, and to compute its inverse.
## The inversion has to be done only once, as the result is cached.
## The first function is a list that enables setting and getting a matrix; as well as
## setting and getting its inverse. The second function computes the matrix inverse.


## This function creates a list of functions that enable the setting and getting of a matrix
## as well as setting and getting the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
  
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This matrix solves the inverse of the matrix created by the previous function.
## If the matrix inverse has already been computed, the function just fetches its value.
## Otherwise, the solve(...) function is called

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
