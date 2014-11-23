##Write the following functions:
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        #This creates the inverse and it will be set to NULL every time makeCacheMatrix is called
        x_inv <- NULL
        
        ##This sets the value of the vector 'set'
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        
        ##This sets the value of the vector 'get' 
        get <- function() x
        
        ## This sets the inverse of the matrix
        setinverse <- function(inverse) x_inv <<- inverse
        
        ## This gets the inverse of the matrix
        getinverse <- function() x_inv
        ## This is a list of the internal functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## This returns a matrix that is the inverse of 'x_inv'
        x_inv <- x$getinverse()
        ##If the inverse has already been set, reutrn it OR...
        if(!is.null(x_inv)) {
                message("getting cached inverse matrix")
                return(x_inv)
        }else {
                ##Get the inverse
                x_inv <- solve(x$get())
                ##set the inverse
                x$setinverse(x_inv)
                #Return the matrix
                x_inv
        }
}