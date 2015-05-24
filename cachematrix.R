@@ -1,15 +1,43 @@
## Put comments here that give an overall description of what your
## functions do
## The first function creates a special matrix that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## will be the cached matrix
  inverse <- NULL 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  ## invert the matrix 
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## Will return the inverted of the matrix
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setsolve(inverse)
  
  ## prints the inverse
  return(inverse)  
}
