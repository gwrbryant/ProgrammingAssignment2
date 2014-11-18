## The two functions below are used to create an object that is used to store a numeric matrix and cache its inverse
## This could be useful when the inverse is required multiple times

## There are 3 different environments involved in implementing these two functions. Using a family tree analogy these are:
##	Parent: The environment in which both functions are called (eg. the global environment)
##	Child: The makeCacheMatrix environment (note that the cacheSolve environment is a different Child of the same Parent)
##	Grandchild: The environments of the functions defined in the makeCacheMatrix environment: set, get, setinverse and getinverse 

## This first function (makeCacheMatrix) provides a set of 4 functions that can be used to store or retrieve the matrix and its inverse
## The functions contained within makeCacheMatrix are available in the environment that first calls it
## Note that makeCacheMatrix only sets up the functions - it does not do any of the actual calculations 

makeCacheMatrix <- function(x = matrix()) {
						
    i <- NULL             ## "i" is a variable in the Child environment that is used to store the matrix inverse	
    set <- function(y) {  ## "set" is used to change the matrix being manipulated in the Child environment  
        x <<- y           ## there is no x variable this environment; "<<-" sets the value of the matrix in the Child envionment  
        i <<- NULL        ## we also need to initialise (to NULL) the inverse cached in the Child environment
    }
    get <- function() x   ## "get" outputs the matrix that has/will have its inverse cached
    setinverse <- function(inverse) i <<- inverse    ## "setinverse" stores the matrix inverse in its cache - variable i in the Child environment 
    getinverse <- function() i                       ## "getinverse" retrieves the matrix inverse from its cache - variable i in Child environment
    list(set = set, get = get,                       ## the function names are now placed in a list so that they can be accessed in the Parent 
         setinverse = setinverse,                    ## environment or by other functions called in the Parent environment
         getinverse = getinverse)
}


## This second function (cacheSolve) provides the inverse of the matrix that is passed to it as its argument
## It either returns the cached inverse, or, if this doesn't exist, it calculates the inverse and caches it for later use.
## cacheSolve use the functions established by makeCacheMatrix, and caches the inverse in a variable (i) found in makeCacheMatrix's (i.e. Child) environment 

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()                   ## call "getinverse" in the Child environment and assign it to a local variable 
        if(!is.null(i)) {                     ## if the inverse already exists then just return it
            message("getting cached data")
            return(i)
        }
        data <- x$get()                       ## otherwise, make the local variable 'data' equal to the matrix in the Child environment 				
        i <- solve(data, ...)                 ## now calculate the inverse of the matrix
        x$setinverse(i)	                      ## and cache the inverse in the Child envrionment by calling setinverse
        i                                     ## finally, output the inverse to the console
}
