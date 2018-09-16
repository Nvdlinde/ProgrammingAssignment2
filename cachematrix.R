## The first function, makeCachematrix creates a special "vector" that cache's the inverse of a matrix
## The second function, cacheSolve, computes the inverse of the special matrix created in the first function

## makeCachematrix creates a special "vector" that cache's the inverse of a matrix
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


## cacheSolve, computes the inverse of the special matrix created in the makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
