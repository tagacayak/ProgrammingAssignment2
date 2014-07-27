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
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



--- orig w comments
# makeCacheMatrix takes a matrix, saved in the private variable x
makeCacheMatrix <- function(x = matrix()) 
{
        # initialize the inverse to NULL during the first call to makeCacheMatrix
        # this is needed because if getinverse() is called immediately after
        # the makeCacheMatrix funciton is constructed, without a call to setinverse()
        # we know we must first calculate the inverse in cacheinverse().  
        m <- NULL 
        
        # function to set a new value for the underlying matrix
        # this invalidates the cached inverse, m
        set <- function(y) 
        {
                # we use the <<- operator to set the value of x and m because we want 
                # to modify x and m defined in the enclosing environment (created 
                # when makeCacheMean() was first called), not in the environment local to set(),
                # in which x and m are undefined.
                x <<- y
                
                # we must reset m to NULL since we are modifying the underlying
                # matrix and the cached value is no longer the valid 
                m <<- NULL  
        }
        
        # getter function for underlying matrix
        get <- function()
        {
                # in R the return value of a function is the last statement.
                # all of these functions could have been written as:
                # return(x), etc... as the last line.
                x
        }
        
        # set the inverse of the matrix x.  Called by cacheSolve,
        # this is pretty weird style, but then so is the whole set up.
        setinverse <- function(inverse) 
        {
                # Again we use the <<- operator because we want to modify the m defined
                # in the enclosing function makeVector(), not the m local to setmean(),
                # which would be undefined (or would create a new variable m local to the
                # scope of setmean()).
                m <<- inverse
        }
        
        # returns the mean.  Will be null if setmean has not been called or
        # if set is called after the last call to setinverse
        getinverse <- function() 
        {
                m
        }
        
        # return value of the makeCacheMatrix function is a list
        # of functions (and variables if we wish) that we want to expose
        # as public.  These are accessed with the $ operator.  Any variables
        # declared inside makeCacheMatrix but not exported as part of this list
        # are private...they are inaccessible to any caller of makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve takes a caching matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) 
{
        # get the mean of the vector defined inside x.
        # we can use the $ operator to access the function since it was
        # defined in the list of function pointers returned by the call to
        # makeVector
        m <- x$getinverse()
        
        # if we've already computed the inverse and stored it via setinverse(),
        # and have not invalidated the cache by calling set(), return the cached
        # version of of the inverse of the matrix.
        if(!is.null(m)) 
        {
                message("getting cached data")
                # we have to explicitly use return here otherwise we'd keep
                # executing the code after the if conditional ends.  Since
                # the cached version is good, just return it and we are done.
                return(m)
        }
        
        # either we haven't computed the cached version yet, or we've called
        # set() previously and invalidated the cache.
        
        # call get() to get the underlying vector
        data <- x$get()
        
        # calculate the inverse of the underlying matrix, passing with it
        # any varargs passed to cacheSolve
        m <- solve(data, ...)
        
        # now set the inverse in x so we cache it and don't need to needlessly
        # recompute it (the point of the assignment and memoization)
        x$setinverse(m)
        
        # return the cached inverse
        m
}