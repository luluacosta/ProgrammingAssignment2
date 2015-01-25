## Caching the inverse of a Matrix
## By Lulu :o)


##Sets a variable called inverso to null. This variable will be where the solved matrix is stored
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
## First it attempts to get the variable called "inverso" (the results of solving the matrix)...
## If it gets a value that is NOT NULL it means that the matrix has already been solved
## and therefore no need to solve it again, so it prints a message "getting cached data"
## and sets the variable "inverso" to that value. Then it prints the value.
## If the value it gets when it attempts to get "inverso" is NULL it means that the matrix
## hasn't been solved, so then it uses "get" to get the matrix (that was made with the previous function)
## and it solves the matrix, stores it in inverso, also sets value of setinverso to the solved matrix so it
## can be used by the other formula.

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

