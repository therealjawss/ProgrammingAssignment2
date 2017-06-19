## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x

        setInverse <- function(newinverse) matrixinverse <<- newinverse
        getInverse <- function() matrixinverse

        matrix(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
        
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getInverse()
        if(!is.null(matrixinverse)){
                message("getting cached data")
                return matrixinverse
        }
        data <- x$get()
        matrixinverse <- solve(x)
        x$setInverse(matrixinverse)
        matrixinverse
}
