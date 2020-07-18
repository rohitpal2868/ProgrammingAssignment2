## In this assignemnt we tried to put inverse the matrix through Cache!!
## Caching the inverse of the matrix (assume that the matrix supplied is always invertible). 

## This function creates a special "matrix" object that can cache its inverse!!

makeCacheMatrix <- function(x = matrix()) {
        inverseit <- NULL
        dataset <- function(y){
                x <<- y
                invnull <<- NULL
        }
        getvalue <- function() {x}
        setinverse <- function(inverse) { inverseit <<- inverse}
        getinverse <- function() {inverseit}
        list(dataset = dataset, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix!!

cacheSolve <- function(x, ...) {
        inverseit <- x$getinverse()
        if(!is.null(inverseit)){
        message("getting cached of the data set")
        return(inverseit)
        }
        mat <- x$getvalue()
        inverseit <- solve(mat, ...)
        x$setinverse(inverseit)
        inverseit
}
