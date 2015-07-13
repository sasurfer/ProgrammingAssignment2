## Functions that cache the inverse of a matrix
#The first function, makeCacheMatrix creates a matrix with a list of 
#functions to:
# 1) set the matrix to be inverted
# 2) get the matrix to be inverted
# 3) set the matrix inverted
# 4) get the matrix inverted


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function given a matrix x calculates its inverse. It checks if 
# it's already been computed. If so it takes the cached value otherwise 
# proceed with the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
