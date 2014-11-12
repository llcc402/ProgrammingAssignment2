
## These functions cache the inverse of a matrix in case of later use, and if the 
## the inverse hasn't been calculated by now they compute it.


## Caching a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function(){
                x
        }
        setInverse <- function(inv){
                inverse <<- inv
        }
        getInverse <- function(){
                inverse
        }
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## Getting the cached inverse or compute it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
