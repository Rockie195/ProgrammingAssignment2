## My functions will find the inverse of a matrix while caching inverses of matrices that were already calculated
## functions do

##The first function creates a matrix that sets the value of the matrix and gets the value of the matrix and then sets the value of the inverse and gets the value of the inverse.

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


## This function calculates the inverse of the matrix above, but first it checks to see if it has been calculated before and if yes it uses the cache value. If not then it calculates the inverse using setinverse.

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
