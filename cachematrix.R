## Since matrix inversion is a costly computation, an efficient way to handle this task is to cache the inverse matrix on the first computation and then to just retrieve it, if the matrix remains unchanged. The following strategy is created for square matrices which are invertible.
## To achieve this, we have two functions:
## makeCacheMatrix and cacheSolve

## makeCacheMatrix has 4 functions:
##      set - stores contents of original matrix and initializes object that shall hold cached invers
##      get - retrieves the original matrix
##      setCacheMatrix - caches the inverse matrix
##      getCacheMatrix - retrieve the cached inverse matrix
##

## cacheSove is basically used to get the resultant inverse of matrix
## Initially, we check if there exists a cached inverse for the current matrix. If the inverse is already computed and cached, we just return this cached inverse
## If this is the first time for computation of inverse and there does not exist a cached version, then the inverse is computed and then it is cached for future usage.

## Consider the following code
## z <- c(1,3,5,7,9,13,12,24,45)
## dim(z) <- c(3,3)
## t <- makeCacheMatrix(z)      creates a special matrix object given a basic matrix z
## cacheSolve(t)                computes inverse of z and then caches it
## cacheSolve(t)                Since the inverse is cached, no need of computation again


## makeCacheMatrix() takes a basic matrix as its argument and creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(inverse) c <<- inverse
        getCacheMatrix <- function() c
        list(set = set, get = get, setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        c <- x$getCacheMatrix()
        if(!is.null(c)) {
                return(c)
        }
        data <- x$get()
        c <- solve(data)
        x$setCacheMatrix(c)
        c
}
