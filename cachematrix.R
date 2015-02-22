
# makeCacheMatrix creates a list of four functions set, get, setinv, getinv.
# set sets the entries of a matrix
# get retrieves the entries of the matrix
# setinv defines a matrix inv in the cache. The purpose of inv is to save the inverse of a matrix so that it does not have to be re-computed.
# getinv retrieves the matrix saved in the cache variable inv.   

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



# cacheSolve computes the inverse of a matrix X. If there is a matrix saved in the cache variable inv, then this matrix is retrieved and output as the inverse matrix of X. Furthermore a message "getting cached inverse" is printed. If there is no matrix saved in inv, then the inverse of X is computed using "solve", and the result is saved in the variable inv.

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
