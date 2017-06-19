## Put comments here that give an overall description of what your
## functions do

## function that initializes default values for matrix, with matrixinverse 
## as the variable that will hold the cached version of the inverse of the 
## given matrix

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



## Function that solves the value of the inverse, if there is none in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getInverse()
        if(!is.null(matrixinverse)){
                message("getting cached data")
                return matrixinverse
        }
        data <- x$get()
        matrixinverse <- solve(x, ...)
        x$setInverse(matrixinverse)
        matrixinverse
}
