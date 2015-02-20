## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        set <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        get <- function() X
        setinv <- function(inverseX) inv <<- inverseX
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        inv <- X$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv
}
