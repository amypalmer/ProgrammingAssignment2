## The following two functions compute and cache the inverse of a matrix 'x'

## makeCacheMatrix takes a matrix 'x' as input and returns a list of 
## functions which can be called using the $ operator. These functions
## are as follows:
##      1. set:    assigns a new value to 'x' and resets the inverse to NULL
##      2. get:    returns the value of 'x'
##      3. setinv: assigns a new value to the inverse variable ('inv')
##      4. getinv: returns the value of 'inv'

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse to NULL during the first call 
        ## to makeCacheMatrix
        inv <- NULL
        
        # function to set a new value for the underlying matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to get the underlying matrix
        get <- function() x
        
        # Function to set the inverse of the vector x
        setinv <- function(inverse) {
                inv <<- inverse
        }
        
        # Function to return the inverse. This will be null if setinv has not 
        # been called
        getinv <- function() {
                inv
        }
        
        # return the value of the makeCacheMatrix function as a list of 
        # functions which are accessible with the $ operator
        list(set = set , get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve takes the matrix created with makeCacheMatrix. Then checks if
## the inverse has been computed for this matrix previously. If it has, this
## function will simply return that value. If it has not, this function will
## compute the inverse and save it within the caching matrix 'x'

cacheSolve <- function(x, ...) {
        # get the value of the inverse of the matrix defined inside 'x'
        inv <- x$getinv()
        
        # if the inverse has already been calculated and stored, simply 
        # return it here along with a message letting the user know it 
        # is a cached value
        if (!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        
        # if it hasn't already been calculated and stored, we will do that here
        
        # first get the matrix stored in 'x'
        data <- x$get()
        
        # calculate the inverse of the matrix, and also pass any other 
        # arguments that were passed to cachmean
        inv <- solve(data, ...)
        
        # now set the inverse in x so that this value is now cached
        x$setinv(inv)
        
        # return the inverse
        inv
}
