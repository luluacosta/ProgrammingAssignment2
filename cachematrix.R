## Caching the inverse of a Matrix
## By Lulu :o)


##Sets a variable called inverso to null. This variable will be the result
##Sets functions to make a matrix, and get/set the value of "inverso"

makeCacheMatrix <- function(x = matrix()) {
    inverso <- NULL
    set <- function(y) {
        x <<- y
        inverso <<- NULL
    }
    get <- function() x
    setinverso <- function(inverso) inverso <<- inverso
    getinverso <- function() inverso
    list(set=set, get=get, setinverso=setinverso, getinverso=getinverso)
}


## This function solves the matrix
## First gets the cached data if it exists, if it does it sets "inverso" to that value
## if it doesn't it solves the matrix and prints that value...

cacheSolve <- function(x, ...) {
    inverso <- x$getinverso() 
    if(!is.null(inverso)) { #if i already have the data no need to re calculate
        message("getting cached data")
        return(inverso)
    }
    datos <- x$get()
    inverso <- solve(datos) #solve gets the inverso of a matrix
    x$setinverso(inverso)
    inverso
}
