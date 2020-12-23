## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                ##Initializes inverse as NULL
        set <- function(y){        
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ##Method to get matrix "x"
        setinv <- function(inverse) inv <<- inverse       ##Set the inverse of the matrix
        getinv <- function() inv                          ##Get the inverse of the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                 
        if(!is.null(inv)){               ##Checking if the inverse is NULL
                message("getting cached data")
                return(inv)               ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        inv <- solve(data, ...)        ##Calculating inverse
        x$setinv(inv)
        inv                            ##Return inverse 
}

