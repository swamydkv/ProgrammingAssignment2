
## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions are used to create a special object that stores a numeric
## matrix and cache's its inverse.

##
## Complete Example:
## > a <- makeCacheMatrix( matrix(c(1,2,12,1), nrow = 2, ncol = 2) );
## > cacheSolve(a)
## .... outputs inverse ...
##               [,1]        [,2]
##       [1,] -0.04347826  0.52173913
##       [2,]  0.08695652 -0.04347826
## > cacheSolve(mcm)
## .... outputs inverse from cache ...
##      getting cached data
##               [,1]        [,2]
##       [1,] -0.04347826  0.52173913
##       [2,]  0.08695652 -0.04347826

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # initialize the stored inverse value to NULL
    inv <- NULL

    # set value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # matrix has changed, reassign to NULL
    }

    # get value of matrix
    get <- function() x
    
    # set inverse of matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # get inverse of matrix
    getinverse <- function() inv

    # return a list containing all functions defined above
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {

    # get inverse
    inv <- x$getinverse()

    # if inverse exists, check if already cached
    # if yes, return cached inverse
    if(!is.null(inv)) {
        message("getting cached data")
    return(inv)
    }

    # if not, get matrix
    data <- x$get()

    # compute inverse of matrix
    inv <- solve(data, ...)

    # cache inverse of matrix
    x$setinverse(inv)

    # return inverse
    inv

}
