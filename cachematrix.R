## These functions will calculate the inverse of an invertible square matrix
## and cache and return the result.  Subsequent calls for the same input matrix
## will return the cached value without recalculating.

## Create a special matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  ## create a matrix

        xI <- NULL                      ## Holding variable for matrix inverse
        set <- function(y) {    
                x <<- y                 ## Save copy of input matrix
                xI <<- NULL             ## Reset matrix Inverse to NULL
        } 
        get <- function() {
                x                       ## Input getter  (get the function input value)
        }
        setInv <- function(solve) {
                xI <<- solve            ## Function setter (function to use to calculate matrix inverse)
        }
        getInv <- function() {
                xI                      ## Result getter (matrix inverse)
        }
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)  ## make internal functions externally available
}


## Compute the inverse of the special matrix. If the inverse has already been
## calculated and the value has not changed, then return the cached value...

cacheSolve <- function(x, ...) {
       
        ## Return a matrix that is the inverse of 'x'
        xI <- x$getInv()                        ## set variable to value of makeCacheMatrix
        
        if(!is.null(xI)) {                      ## check if the inverse (xI) already exists
                message("getting cached data")  ## if inverse already exists, report to console
                return(xI)                      ## return the cached inverse matrix value
        }
        
        data <- x$get()                         ## set 'data' to the input matrix from makeCacheMarix
        xI <- solve(data, ...)                  ## calculate the inverse of the matrix
        x$setInv(xI)                            ## store the inverse with the input matrix
        xI                                      ## return the inverse of the input matrix
}
