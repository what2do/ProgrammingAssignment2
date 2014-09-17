## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below are 2 functions that cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It is a list containing a function to:
## - set the value of the vector
## - get the value of the vector
## - set the value of the mean
## - get the value of the mean

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


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache. Otherwise, it computes the inverse and sets the value the cache 
## via the setinverse function.
## Assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


## Test run output
## > source("cachematrix.R")
## > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > amatrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(amatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > amatrix$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(amatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
## > cacheSolve(amatrix)
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > amatrix$get()
##      [,1] [,2]
## [1,]    0   99
## [2,]    5   66
## > amatrix$getinverse()
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## > cacheSolve(amatrix)
## getting cached data
##             [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
