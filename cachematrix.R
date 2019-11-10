## This is a demonstration of the <<- operator which can be used 
## to assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are 
## used to create a special object that stores a matrix and 
## cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set  <- function(y) {
                x    <<- y
                xInv <<- NULL
        }
        get    <- function() x
        setinv <- function(inv) xInv <<- inv
        getinv <- function() xInv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

## The second function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(xCache, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- xCache$getinv()
        if(!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
        }
        data <- xCache$get()
        xInv <- solve(data, ...)
        xCache$setinv(xInv)
        xInv
}

## Test.
x <- matrix(runif(9), nrow = 3, ncol = 3)
xCache <- makeCacheMatrix(x)
(xCache$get())

## Calling cacheSolve() the first time
(xInv   <- cacheSolve(xCache))

## Calling cacheSolve() the second time
(xInv   <- cacheSolve(xCache))

(x %*% xInv) ## Should be very close to identity matrix.
