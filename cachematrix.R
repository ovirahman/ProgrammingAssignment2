##The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix
##The second function inverse the matrix created with the above function.
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of 
##the inverse matrix in the cache via the setinverse function.




## The following function defines the getter and setter functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## inverse matrix checkhing cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data for inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
