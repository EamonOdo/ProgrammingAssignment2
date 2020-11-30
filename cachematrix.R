## Programming Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly

## Below is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.Write a short comment describing this function.

cacheSolve <- function(x, ...) {
        j <- x$getInverse()
        if(!is.null(j)){
              message("getting cached data")
              return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}
