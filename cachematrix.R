# This function is to compute the inverse of matrix with caching the value for 
# future use.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to:
### 1. set the value of matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse

## cacheSolve calculates the inverse of the special "matrix" created with the 
## above function. It checks to see if the mean has already been calculated. If
## so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setinverse function.


#This function is to create a "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y){
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinverse = function(solve) m <<- solve
    getinverse = function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function is to get the inverse of matrix
cacheSolve = function(x,...){
    m = x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data,...)
    x$setinverse(m)
    m
}
