## This function creates a "special" matrix which is really a list with 
## functions that allows it to cache the inverse of a matrix.

## This function creates a "special" matrix that is really a list containing 
## functions set, get, setinverse, and getinverse that allows for caching of
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the list established in the makeCacheMatrix to either
## return the cached inverse or to calculate and store the inverse of the
## matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
