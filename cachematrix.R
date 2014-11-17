## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This method creates makeCacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m    
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}  


## Write a short comment describing this function
## This method checks if the matrix inverse is already cacluated if yes return that or return inversed matrix and cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
