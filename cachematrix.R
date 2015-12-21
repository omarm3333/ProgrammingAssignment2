## Description: in order to reduce computation cycles, this two functions helps to create, calculate and mantain 
## the inverse of a sourced matrix an keep the result cached until source matric changes

## makeCacheMatrix: Returns an special vector to initialize a source matrix, and functions to access its content
##      Arguments:
##              x: Source matrix
##
##      Returns: List of available functions to access inverted source matrix
## 
makeCacheMatrix <- function(x = matrix()) {
        ## Init cacheable inverse matrix
        ix <- NULL
        
        ## set(y): Sets value of source matrix in called environment and clears inversed matrix
        ##      x: Source matrix
        ##      ix: Inversed matrix
        ##
        ## NOTE: As a recommended optimization, in this function the inverse matrix ('ix') could be calculated as
        ##      soon as source matrix ('x') has changed and inverse matrix is cached immediatly.
        ##      In such case cacheSolve() might not be required.
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        
        ## get(): Returns source matrix
        get <- function() x
        
        ## setInverse(im): Assign the passed inverse matrix 'im' to local inversed matrix 'ix' to cache it
        setInverse <- function(im) ix <<- im
        
        ## getInverse(): Returns the cached inversed matrix 'ix'
        getInverse <- function() ix
        
        ## Set the returns access functions of makeCacheMatrix()
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve: Computes and return the inverse of source matrix. The inversed matrix result is cached for
##      further use until source matrix is changed
##      Arguments:
##              x: Special 'makeCacheMatrix' vector with cached matrixes (source and inverse) and 
##                      access functions
##              ...: Additional parameters to pass on to solve function
##
##      Returns: A matrix that is the inverse of source matrix ('x')
##              
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Retrieve cached inverse from Special makeCacheMatrix vector
        ix <- x$getInverse()
        
        ## Checks if inverse is NULL (not calculated and cached yet), if not, return cached inverse matrix
        if(!is.null(ix)) {
                ## Inverse matrix cached already, return cached value
                message("Getting cached data")
                return(ix)
        }
        ## Inverse matrix not cached, retreive source matrix in 'data'
        data <- x$get()
        
        ## Checks if data is not a matrix or is an empty one, return NA if so
        if(!is.matrix(data) || is.na(data)) return (NA)
        
        ## Calculate inverse matrix 'ix' with solve() passing on additional parameters of '...'
        ix <- solve(data, ...)
        
        ## Set in special vector the resulting inverse matrix to be cached
        x$setInverse(ix)
        
        ## Return inverted matrix
        ix
}
