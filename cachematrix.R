## Computing inverse of large matrices is memory intensive
## Therefore the task is to compute the inverse of a matrix by caching
## the computed values. 
## Two functions are used - makeCacheMatrix() and cacheSolve()


## makeCacheMatrix - The function inputs a matrix ans 
## Sets the matrix in a cache
## You can then get the matrix

makeCacheMatrix <- function(x) {
        m   <- NULL 
        set <- function(y) {
                x <<- y
		m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve - The function solves the inverse of the matrix x
## Checks if the inverse matrix from cache exists, if so, it only retrieves it
## If matrix inverse is not null, then solve() function computes the inverse m

cacheSolve <- function(x, ...) {

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
