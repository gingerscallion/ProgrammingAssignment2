## makeCacheMatrix Creates a matrix obj that can cache its inverse
## cacheSolve Computes inverse of the matrix returned by makeCacheMatrix unless already computed and cached

## Creates a matrix obj that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- matrix()
set <- function(y){
        x <<- y
        m <<- matrix()
}
get <- function() x
setinv <- function(inv) m <<- inv
getinv <- function() m
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Computes inverse of the matrix returned by makeCacheMatrix unless already computed and cached


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        if(!is.matrix(m))
        {
                message("Getting cached inverted matrix ")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
