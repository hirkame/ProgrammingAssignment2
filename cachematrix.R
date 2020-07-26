## Matrix inversion with Caching

## Make a matrix object which can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list (set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## If there is cache, just get the inverse from cache. If not, do the calculation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("Got the inverse of x from cache.")
        return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}

## test code
## ex <- matrix(c(-3,5,1,0), nrow=2, ncol=2)
## another <- makeCacheMatrix(ex)
## inv <- cacheSolve(another)
## inv <- cacheSolve(another)
## ex
## inv
