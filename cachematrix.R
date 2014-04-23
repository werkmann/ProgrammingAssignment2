## TODO: 
## Put comments here that give an overall description of what your
## functions do


## This function converts a convetional R matrix into a special matrix that is 
## able to cache the compute-intensive inverse matrix.
##
## The function returns a list containing fields that store function values: 
##   * set        -> stores a new matrix value and clears the cache
##   * get        -> returns the original matrix 
##   * setinverse -> stores a new inserve matrix value cache 
##   * getinverse -> returns the cached inverse matrix, NULL if not cached
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  matrix  <- NULL
  
  set <- function(m) {
    
    matrix  <<- m     # This variable stores the original matrix
    inverse <<- NULL  # This variable stores the inverse matrix
  }
  
  get <- function() matrix
  
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  set(x)
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function caculates the inverse matrix of a special matrix m created by
## makeCacheMatrix. If the inverse has not been calculated, it calculates it 
## using the solve function and stores the result in a cache variable. If the
## inverse has already been calculated it simply returns the cached inverse 
## matrix. The variable function arguments will be directly passed to the solve
## function.
cacheSolve <- function(m, ...) {
  
  # First check wether the inverse has been cached, 
  # if so simply return it.
  inverse <- m$getinverse()
  if(!is.null(inverse)) {
    
    message('The inverse has already been calculated')
    
    return(inverse)
  }
  
  message('No inverse matrix cached, calculating inverse')

  matrix <- m$get()
  
  # It is necessary to re-specify the non-related function arguments because 
  # the arguments shall be redistributed from the parent function to the solve
  # function.
  #inverse <- solve(matrix, b=NA, LINPACK=FALSE, ...)  
  inverse <- solve(matrix)  

  message('Caching the inverse matrix')
  
  m$setinverse(inverse)
  
  return(inverse)
}
