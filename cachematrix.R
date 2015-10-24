## Creates a matrix and caches its inverse

## Creates a list of functions to set and get input matrix and set and get its inverse.

makeCacheMatrix <- function(inM = matrix()) {
      i <- NULL  
      setmat <- function(y) {
            inM <<- y                            ## Sets input matrix to y and caches it.
            i <<- NULL                           ## Initializes matrix to store inverse.
      }
      getmat <- function() inM                   ## getmat retrieves input matrix.
      setinv <- function(inv) i <<- inv          ## setinv caches inverse matrix.
      getinv <- function() i                     ## getinv retrieves (possibly empty) inverse matrix.
      list(setmat = setmat, getmat = getmat,
           setinv = setinv,
           getinv = getinv)
}


## Computes inverse of matrix object created by makeCacheMatrix, if inverse not already
exists in cache; else return cached inverse.

cacheSolve <- function(x, ...) {
      i <- x$getinv()                        ## Retrieves (possibly empty) inverse matrix.
      if(!is.null(i)) {                      ## Checks if retrieved inverse matrix is empty.
            message("getting cached data")
            return(i)                        ## If not, then it's from cache and is returned.
      }
      data <- x$getmat()                     ## Else input matrix is retrieved and its inverse compputed.
      i <- solve(data, ...)
      x$setinv(i)                            ## Computed inverse is cached and displayed.
      i
}
