## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv)  i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve:  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), 
##              then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached inverse matrix")
        return(i)  ##  return cached inverse matrix
    }
    
    i <- solve(x$get(), ...)  ## solve for inverse of matrix x.  Default b parameter is identity matrix
    x$setinv(i)    ## save new inverse matrix to CacheMatirx object
    i   ##  return newly calculated inverse matrix
}
