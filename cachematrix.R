## User input : a matrix(e.g. m = matrix())
## with the given matrix, these functions return the inverse of that matrix.

## makeCacheMatrix function
##  creates a Cache Matrix Object(stores the original matrix & its inverse)
##  4 functions included----------------------
##  |-setter and getter for original matrix  |
##  |-setter and getter for original inverse |
##  ------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    #Initialize the matrix
    i <- NULL
    
    #First function : set(y)
    #Stores the original matrix and initialize the inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #Second function : get()
    #Returns the original matrix
    get <- function() x
    
    #Third function : setInverse(inverse)
    #Stores the inverse matrix and initialize the inverse matrix
    setInverse <- function(inverse) i <<- inverse
    
    #Fourth function : getInverse()
    #Returns the inversed matrix
    getInverse <- function() i
    
    #Store all the functions made so far
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Return a matrix that is the inverse of 'x' and store it in cache
##  when called upon the same cache matrix, instead of calculating
##  the inverse, cached inversed matrix is returned
cacheSolve <- function(x, ...) {
        
    #try to retrieve a cached inverse matrix
    i <- x$getInverse()
    
    #if there is a cached data return that data
    if(!is.null(i)) {
        message("Cache found... retrieving cached data")
        return(i)
    }
    
    #if there is no cached inverse matrix calculate one
    original <- x$get()
    i <- solve(original,...)
    
    #set the inverse matrix
    x$setInverse(i)
    
    #return the inverse matrix
    i        
}