## Put comments here that give an overall description of what your
## functions do
## written by Erik McCullen
## Write a short comment describing this function
## function makeCacheMatrix is to set the value of a matrix, get the value of the matrix,
## set the value of the inverse of the matrix, and get the value of 
## the matrix
## function cacheSolve is to retrieve cached data if inverse already calculated
## or get the inverse if it has not previously been calculated
makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
