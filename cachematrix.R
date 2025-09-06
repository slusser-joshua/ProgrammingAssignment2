## The two functions below serve to cache the inverse of an input matrix, thus saving
## the computation time of computing the inverse multiple times

## This first function makeCacheMatrix creates a list which contains a function to
## Set and get the value of the matrix
## Set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(inverse) invMatrix <<- inverse
    getInvMatrix <- function() invMatrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## After running the makeCacheMatrix function, input the resulting list into the
## function below, which will check to see if the inverse of the matrix 'x' has 
## already been created.  If so, this function will obtain the inverse of the matrix
## from the cache.  If not, this function will compute the inverse of the matrix
## using the solve() function.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInvMatrix()
    ## Check to see if inverse has already been created
    if(!is.null(invMatrix)) {
        message("getting cached inverse matrix")
        return(invMatrix)
    }
    ## If inverse has not been created, compute the inverse here
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInvMatrix(invMatrix)
    invMatrix   
}
