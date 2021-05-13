## R Programming Assignment 2
##

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ##initialize variable m
    ##function to reset m to null and define x matrix as y matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ##get matrix x
    get <- function() x
    ##set inverse of m to variable inverse
    setinv <- function(solve) m <<- solve
    ##get inverse of matrix m 
    getinv <- function() m
    ##get a list of the set function, get function, setinv, and getinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then`cacheSolve` should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##assign matrix the value of getinv
    m <- x$getinv()
    ##if m is not NULL then return m
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##if m is NULL calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
