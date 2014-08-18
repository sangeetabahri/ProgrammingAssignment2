## makeCacheMatrix function creates a special matrix, which is a list containg functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

## makeCacheMatrix function - Creates a speical matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) { ## function to set matrix
        x <<- y
        iv <<- NULL
    }
    get <- function() x  ## function to get matrix
    # In R solve does the inverse of the matrix
    setinverse <- function(solve) iv <<- solve  ## function caches inverse of matrix
    getinverse <- function() iv                 ## function fetches the matrix inverse
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
    ##checks if inverse is not null, which means inverse aleady calculates, and cached
    ## so return cache value. 
    if(!is.null(iv)) { 
        message("getting cached data")
        return(iv)
    }
    ##This piece will only get executed if inverse was null
    ## fetch the matrix, and inverse and cache 
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv
}
