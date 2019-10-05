## This overall code allows to create a kind of matrix that can to store
## a cache inverse, this inverse its assigned by the option setinverse and 
## when you store this value in the firstime you don't have to calculate it
## again. This code have 2 parts, the first it is the basic functions and
## the second it is where the calculation is done.


## This function create the basic operation to the special matrix
## like set, get, getinverse and setinverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(m_inverse) inv <<- m_inverse ##me
        getinverse <- function () inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculate the inverse of the matrix only if the inverse
## not had been calculate before.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            print("getting cached data")
            return(inv)
            
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
