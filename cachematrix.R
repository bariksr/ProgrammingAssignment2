#The following two functions are used to create an object that
# stores a matrix and caches it's inverse

#The first function, makeCacheMatrix, creates a list containing a function to:
# 1. Set the value of a matrix 
# 2. Get the value of a matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

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


## Write a short comment describing this function
# The second function calculates the inverse of the matrix.
# It first checks to see if the inverse has already been calculated, and if so,
# gets it from the cache and returns it. If it has not been calculated, 
# it calculates the inverse and sets the value of the inverse in the 
# cache using the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
