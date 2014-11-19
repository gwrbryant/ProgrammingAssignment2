## The two functions below are used to create an object that is used to store   
## a numeric matrix and cache its inverse.
## This could be useful when the inverse is required multiple times

## There are 3 different environments involved  
## Using a family tree analogy these are:
##    Parent: The environment in which both functions are called (eg. global)
##    Child: The makeCacheMatrix environment
##           (the cacheSolve environment is a different Child but same Parent)
##    Grandchild: That of the functions defined in the Child environment: 
##                set, get, setinverse and getinverse 

## This first function (makeCacheMatrix) provides a set of 4 functions that  
## can be used to store or retrieve the matrix and its inverse. The functions
## contained within makeCacheMatrix are available in the environment that 
## first calls it. Note that makeCacheMatrix only sets up the functions
##  - it does not do any of the actual calculations 
## The 4 functions are:
## "set" sets the value of the matrix
## "get" gets the value of the matrix
## "setinverse" sets the value of the matrix inverse
## "getinverse" gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL    ## a variable in the Child environment used for the inverse             
    set <- function(y) {    ## change the matrix being manipulated
        x <<- y    ## "<<-" sets the value of x in the Child envionment  
        i <<- NULL    ## initialise (to NULL) any existing cached inverse
    }
    get <- function() x   ## output the matrix being manipulated
    setinverse <- function(inverse) i <<- inverse    ## cache the inverse in i
    getinverse <- function() i    ## retrieve the inverse from its cache (i)
    list(set = set, get = get,    ## place the function names in a list 
         setinverse = setinverse,    ## so they can be accessed in/from  
         getinverse = getinverse)    ## the Parent environment 
}


## This second function (cacheSolve) provides the inverse of the matrix 
## that is passed as its argument; It either returns the cached inverse, 
## or, if this doesn't exist, it calculates the inverse and caches it.
## cacheSolve use the functions established by makeCacheMatrix,and caches 
## the inverse in the variable (i) in the Child environment defined above.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()    ## get the inverse from the Child environment 
        if(!is.null(i)) {    ## if the inverse already exists 
            message("getting cached data")    ## tell us and
            return(i)    ## then just return it to the calling environment
        }
        data <- x$get()    ## otherwise, make a local copy of the matrix, 				
        i <- solve(data, ...)    ## calculate its inverse, 
        x$setinverse(i)    ## and cache the inverse in the Child environment
        i    ## finally, output the inverse to the console
}
