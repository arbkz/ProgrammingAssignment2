# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# The Cache Matrix object consists of 2 matrices - X which will contain the matrix you want to invert 
# and I which will contain it's inverse 

makeCacheMatrix <- function(X = matrix()) {
    I <- NULL
   
    # setter function - this will set the value of Matrix object X based on the passed argument Y 
    # it also sets the inverse matrix I to NULL for later use and to avoid throwing an errors if the inverse is called with no init
    # the double get << shows that X and I were defined in the calling environment not within the function or as an argument
    # a single get would create a variable X with scope that only exist inside the set function
    set <- function(Y) {
        X <<- Y
        I <<- NULL
    }
    
    # getter function - returns the instance matrix X
    # This can be created by passing it when calling makeCM function or using the setter above 
    
    get <- function() X
    
    #setter for inverse - set the value of matrix object
    setinverse <- function(inverse) I <<- inverse
    
    #getter for inverse - returns the instance inverse I
    getinverse <- function() I
    
    #return created CacheMatrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(X, ...) {
    I <- X$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- X$get()
    I <- solve(data, ...)
    X$setinverse(I)
    I
}



