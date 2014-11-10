## Put comments here that give an overall description of what your
## functions do

# list of the makeCacheMatrix functions:
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL # init var invmatrix
  
  ## Function to set matrix
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  ## Function to get matrix
  get <- function() x
  
  ## Function to set inverse matrix
  setinverse <- function(inverse) invmatrix <<- inverse
  
  ## Function to get  inverse matrix
  getinverse <- function() invmatrix
  
  ## List of the functions (methods)
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Some comment

## Write a short comment describing this function

## Compute the inverse of the matrix returned makeCacheMatrix function above
##  If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache
##

cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data.")
    return(invmatrix)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
