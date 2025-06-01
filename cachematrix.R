## Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return a list of the four functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  
  
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  mat <- x$get()
  inv <- solve(mat, ...)  
  x$setinverse(inv)       
  inv                     
}
