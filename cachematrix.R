## makeCacheMatrix creates a special matrix object that can cache its inverse.
## cacheSolve computes the inverse of the special matrix returned by the makeCacheMatrix function.


## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It creates and returns a list of functions 'set', 'get', 'setinverse' and 'getinverse'
## and makes them public in order to cache the inverse of the special matrix object.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL 
        set <- function(y){
                x <<- y
                m <<- NULL  
        }
        
        get <- function(){
                x
        }
        
        setinverse <- function(inverse){
                m <<- inverse
        }
        
        getinverse <- function(){
                m
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix returned by the makeCacheMatrix function.
## If the inverse of the special matrix has already been calculated, and the matrix has not changed,
## then cacheSolve retrieves the inverse matrix of 'x' from cache, otherwise it calculates the inverse.
## It returns the inverse of the special matrix.

cacheSolve <- function(x, ...){
        
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



