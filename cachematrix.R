

##  the file includes two functions as below
##  the mackCacheMatrix function contains a list of four functions,get,set,getinv,setinv

makeCacheMatrix <- function(x = matrix()) 
{
    v <- NULL
    set <- function(y)       ##set function,set the original matrix and set inverse be NULL
    {
        x <<- y
        v <<- NULL
    }
    get <- function() x      ##get function,get the original matrix
    setinv <- function(inv) v <<- inv
    getinv <- function()    v
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the cacheSolve gives the inverse of a matrix using cache to save time

cacheSolve <- function(x, ...) 
{
    v <- x$getinv()               ##first try to get result from cache
    if(!is.null(v))               ## if found, return the result
    {
        message("getting cached data")
        return(v)
    }                             ## if not found, calculate and save it in cache
    data <- x$get()
    v <- solve(data)
    x$setinv(v)
    v                             ## Return a matrix that is the inverse of 'x'
}
