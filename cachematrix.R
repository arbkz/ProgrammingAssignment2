
# Function to find and cache the inverse of a matrix 
#  x is used to store the matrix, i is used to store it's inverse

makeCacheMatrix <- function(x = matrix()) {

  
  #  for each new matrix object, init the inverse to NULL
  i <- NULL

  # setter function
  # after you create a new CacheMatrix object, this function set it's value.
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # getter function
  get <- function() x
  
  #inverse setter and getter
  
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

