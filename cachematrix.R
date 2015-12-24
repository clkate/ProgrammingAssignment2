## Overall Description:Cache the inverse of a matrix.

## Creates a special list that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  ## A funtion to set the value of the matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## A function to get the value of the matrix.
  get <- function() x
  
  ## A function to set the value of inverse matrix.
  setinverse <-function(inverse) i <<- inverse
  
  ## A function to get the value of inverse matrix.
  getinverse <-function() i
  ## Create a special list which contains the above functions.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Computes the inverse value of matrix.
cacheSolve <- function(x, ...) {
  ## Try to retreive the inverse of the matrix from the special list.
  i <- x$getinverse()
  if(!is.null(i)) {
  ## If inverse of the matrix has already been calculated, returned the cached data.
    message("getting cached data")
    return(i)
  }
  ## Otherwise, it computes the inverse and set the result in the cache. 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
