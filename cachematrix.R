## Assignment 2: Caching the Inverse of a Matrix
## Author: Hong Xu

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(x_inverse) x_inv <<- x_inverse
    get_inverse <- function() x_inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x_inv <- x$get_inverse()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    mat <- x$get()
    x_inv <- solve(mat, ...)
    x$set_inverse(x_inv)
    x_inv
}

# Test
mat <- makeCacheMatrix(matrix(rnorm(200*200),200,200))
mat$get()
mat$get_inverse()
cacheSolve(mat)
mat$get_inverse()
cacheSolve(mat)
mat <- makeCacheMatrix(matrix(rnorm(10*10),10,10))
mat$get()
mat$get_inverse()
cacheSolve(mat)
mat$get_inverse()
cacheSolve(mat)
