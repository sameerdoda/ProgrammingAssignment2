## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object to cache the inverse of the matrix
## It is a list containing a function to 
##      set the value of the matrix, 
##      get the value of the matrix,
##      set the inverse of the matrix,
##      get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set=set, get=get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function first checks if the inverse has already been computed
## If so, it returns the cached value else
## It calculates the invrse using the solve function and returns the value

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
