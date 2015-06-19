## This file contatins two functions for efficiently evaluating the inverse of a Matrix.
## One method sets the matrix value and can cache its inverse. The other reports the cached 
## inverse if it exists, and calculates it if it does not.

## This function represents represents a Matrix and can cache the inverse of that matrix. 
## It contains getter and setter methods for the matrix and for its inverse.
makeCacheMatrix <- function(M = matrix()) {
  invM <- NULL
  
  set  <- function(mat) {
    M    <<- mat
    invM <<- NULL
  }
  get <- function() M
  
  setInv <- function(inv) invM <<- inv
  getInv <- function() invM
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of the matrix stored in a matrix 
## "object" specified by makeCacheMatrix. If this inverse is cached then 
## that is returned, if not then the inverse is calculated and returned. 
cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## If inverse is cached return that, if not calculate it.
  
  invM <- M$getInv()
  if(!is.null(invM)){
    message("Getting cached inverse.")
    return(invM)
  }
  
  mat = M$get()
  invM <- solve(mat)
  M$setInv(invM)
  invM
}
