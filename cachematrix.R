## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two functions makeCacheMatrix, makeCacheMatix
## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS) is used to calculate inverse for non squared as well as squared metrics
library(MASS)
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x} # to get matrix (x)
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function(){inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get cache data

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...) # calculates inverse value
        x$setInverse(inv)
        inv
}
