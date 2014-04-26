## The two functions work in concert to return the inverse of a matrix.  The inverse 
## computation is only performed if the computation hasn't already been performed.  
## If the computation has already been performed, the matrix inverse will simply
## be retrieved from the cache.



## makeCacheMatrix essentially creates a special list that houses the 
## attributes of a particular matrix.  The special list contains 
## the matrix definition(number of rows, colums, etc.) in addition to the cached inverse
## if it has already has been calculated.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## cacheSolve first checks if the inverse has already been computed.
## If so, the function prints "getting cached data" and returns the cached data. 
## Otherwise, the function computes the inverse of the matrix, sets it to the
## special list genererated by makeCacheMatrix and prints the inverse matrix solution. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


