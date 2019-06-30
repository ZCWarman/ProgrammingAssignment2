## Put comments here that give an overall description of what your
## functions do

## The goal of this assignment is to write two functions ("makeCacheMatrix" &
## "cacheSolve") that cache the inverse of a matrix.

## Write a short comment describing this function
## "makeCacheMatrix" is a function that creates a special "matrix" which
## caches the inverse for the input (an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## "cacheSolve" is a function which computes the inverse of the special "matrix"
## created by "makeCacheMatix." Should the inverse already be calculated or present,
## (and the matrix is unchanged), then the cacheSolve function should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("retrieving data from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse      
}


