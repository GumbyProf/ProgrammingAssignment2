## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix allows for an inverse to be put into a cache variable for
##  easy of computing later

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               #start with an empty m
    set <- function(y) {    #make an function
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve() #do the magic
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    #define the list of things to return
}


## Write a short comment describing this function

##cacheSolve returns a previously inverted matrix, if it exists
##cacheSolve inverts a matrix, if it wasn't already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {           #What to do if the inverse was already found
        message("getting cached data")
        return(m)
    }
    data <- x$get()      #what to do if the inverse has not yet been found.
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
