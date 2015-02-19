## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {

  ## intialization
  inv <- NULL
  
  ## sets new matrix
  set <- function(y)
  {
    x <- y
    inv <- NULL
  }
  ## gets current matrix
  get <- function()
  {
    x
  }
  ## gets cached inverse matrix
  getInverse <- function()
  {
    inv
  }
  ## puts new inverse matrix value to cache
  ## be carrefull that you sets correct inverse matrix
  ## try to use more reliable function calcInverse (see below)
  setInverse <- function(inverseMatrix)
  {
    inv <<- inverseMatrix
  }
  ## this function returns inverse value of underlying  matrix.
  ## It performs real calcualtion, if inverse matrix has not been calculated before.
  ## If value of inverse matrix has been cached, then it returns it immediately.
  calcInverse <- function(...)
  {
    if(is.null(inv))
    {
      inverseMatrix <- solve(x, ...)
      inv <<- inverseMatrix
    }
    else
    {
      message("inverse matrix is alredy calculated")  
    }
    return (inv);
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, calcInverse = calcInverse)	
}


## Write a short comment describing this function

## This function calculates inverse value of underlying matrix or
## returns cached value, if calculation was already performed and
## wrapped matrix was not changed since the last real calculation.
## This implementation is naive and expects that matrix is always
## invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$getInverse()
  if(!is.null(result))
  {
    message("getting cached data")
    return (result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setInverse(result)
  result
}
