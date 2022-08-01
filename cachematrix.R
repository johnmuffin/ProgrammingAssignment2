## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Takes a matrix "x", and creates a special object for it with 4 functions.
# set = which allows you to set the matrix within the object
# get = which simply returns the matrix found within the object
# setInvMatrix = which allows you to set the inverse matrix within the object
# getInvMatrix = which simply returns the inverse matrix within the object

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <- y
        invMatrix <- NULL
    }
    get <- function() x
    setInvMatrix <- function(inv) invMatrix <<- inv
    getInvMatrix <- function() invMatrix
    list (set = set, get = get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## Write a short comment describing this function
# Checks if within the special matrix object that if the Inverse matrix is calculated within the object
# if yes, then the value of the inverse matrix that has been calculated is returned
# if no, then the inverse matrix is calculated based on the original matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setInvMatrix(invMatrix)
    invMatrix
}

