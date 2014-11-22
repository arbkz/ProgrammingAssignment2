## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a Cached matrix object and methods

makeCacheMatrix <- function(x = matrix()) {

  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}



## function to find the inverse of the Matrix x and Cache it's value for next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

