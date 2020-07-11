## Script that cache potentially time-consuming computations.

## Creates a special "matrix", which is really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
        inv_ma <- NULL
        set <- function(y) {
            x <<- y
            inv_ma <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_ma <<- inverse
        getinverse <- function() inv_ma
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## Calculates the inverse of the special "matrix"
## created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_ma <- x$getinverse()
        if(!is.null(inv_ma)) {
            message("getting cached data")
            return(inv_ma)
        }
        data <- x$get()
        inv_ma <- solve(data, ...)
        x$setinverse(inv_ma)
        inv_ma
}


x <- matrix(1:4, nrow = 2, ncol = 2)
m <- makeCacheMatrix(x)
cacheSolve(m)

