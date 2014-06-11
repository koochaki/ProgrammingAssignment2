## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and then creates a list of functions that 
## allows for caching of the 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    z <<- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- z$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- z$get()
    m <- solve(data)
    z$setinverse(m)
    m
}

y <- matrix(1:4,2,2)
makeCacheMatrix(y)
cacheSolve(y)
