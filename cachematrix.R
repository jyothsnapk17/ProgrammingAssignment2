## framework to cache the inverse of a matrix when possible

## makeCacheMatrix takes a matrix as input and returns a list with the 
## getter and setter functions

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) {
                x <<- y
                inverse <<- NULL 
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(inv) {
#                 message("setting inverse")
                inverse <<- inv
#                 print(inverse)
        }
        
        printInverse <- function() {
                message("Stored Inverse Value")
                print(inverse)
        }
        
        getInverse <- function() {
#                 message("returning inverse")
#                 print(inverse)
                inverse 
        }
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse, printInverse = printInverse)
}


## cacheSolve takes the list created by the previous function and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("cached results retrieved")
                return(inv)
        }
        message("calculating inverse")
        data <- x$get() 
        inv <- solve(data, ...)
#         print(inv)
        x$setInverse(inv)
        inv 
}
