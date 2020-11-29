## makeCacheMatrixn function creates a matrix object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above


makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {invrs <<- inverse}
        getInverse <- function() {invrs} 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of 'x' 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat,...)
        x$setInverse(invrs)
        invrs
}
