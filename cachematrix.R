## makeCacheMatrix function creates a special matrix, which is a list containg functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

## makeCacheMatrix function - Creates a speical matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) iv <<- solve
    getinverse <- function() iv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function - Computes the inverse. 
## If the inverse has already been calculated ( and matrix has not changed),
## then the function retrieve the inverse from the cache, 
## If the matrix is changed it calculate the inverse, and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv
}
