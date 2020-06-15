## Functions to cache th inverse of a matrix as it is usually a costly computation
## Caching the inverse of a matrix rather than computing it repeatedly may be beneficial. 

library(matlib)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x) == nrow(x) && det(x) != 0) {
    # Setting m to NULL as placeholder for future value - matrix in this case.
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    # Returns the matrix x
    get <- function() x
    # sets the inverse of matrix x to m
    setinvmat <- function(x) m <<- x  #  inv(x)
    # Returns m - inverse of matrix x
    getinvmat <- function() m
    # the 'special vector' containing all of the functions just defined
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
  }else{
    return(message("The matrix is not invertible."))
  }
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmat()
  if(!is.null(m) && identical(m, x)) {
    message("getting cached data")
    return(m)
  }
  # get input matrix
  data <- x$get()
  # cache the input matrix
  x$set(data)
  # get inverse of input
  m <- solve(data, ...)
  # set inverse to cache
  x$setinvmat(m)
  # return the inverse matrix
  m
  
}

testmat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
testcache <- makeCacheMatrix(testmat)
cacheSolve(testcache)
