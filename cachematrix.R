#The overall aim of this script is to speed up the sometimes costly computation associated with matrix inversion by caching the inverse of the matrix rather than computing it repitively. 

# The makeCacheMatrix function creases a special "matrix" which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The function below computers the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, cacheSolve should retrieve the inverse from the cache. 

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
