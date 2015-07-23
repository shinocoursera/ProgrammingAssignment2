## This file contains 2 functions.  One is makeCacheMatrix and other is cacheSolve.
## Using combination of two functions, it increase the efficiency by caching
## inversable matrix that is used at least once.  
## Due to inversing matrix is computation heavy opertaion, caching of the
## inverted matrix rather than computing everytime would be more efficient.

## The function allows to create inversable matrix and also gets inversed matrix
## @param x: takes matrix and in this case, assumed that the matrix passed is
##           invertable matrix.
## @return list: returns the list of setter and getter functions of 
##               inversable matrix and inversed matrix
makeCacheMatrix <- function(x = matrix()) {

  ## Initalizing inverse matrix to be NULL
  inversedMatrix <- NULL
  
  ## Setter of an Inversable Matrix
  setMatrix <- function(inversableMatrix) {
    x <<- inversableMatrix
    inversedMatrix <<- NULL
  }
  
  ## Getter of an Inversable Matrix
  getMatrix <- function() x
  
  ## Setter of Inversed Matrix
  setInversedMatrix <- function( solve ) inversedMatrix <<- solve
  
  ## Getter of Inversed Matrix
  getInversedMatrix <- function() inversedMatrix
  
  ## returns the list of functions to set and get the inversable matrix and set and get 
  ## the inversed matrix.
  list( setMatrix = setMatrix, getMatrix = getMatrix, 
        setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix )

}


## The function generates inversedMatrix through cached inverted marix or computed
## inverted matrix if invertable matrix is initated to be inverted for the first time.
## @param x: takes list of functions to set and get inversable/inverted matrix.
## @return inversedMatrix: returns inversed matrix from either cache if already exists 
## or by computation. 
cacheSolve <- function(x, ...) {
  
  ## Get a matrix that is already inverted.
  inversedMatrix <- x$getInversedMatrix()
  
  ## Since value is already cached, it returns
  ## the data from cache.
  if(!is.null(inversedMatrix)){
    message("getting cached data")
    return (inversedMatrix)
  }
  
  ## Since inverted matrix was NULL, get original data
  ## before inverting matrix
  matrixBeforeInverse <- x$getMatrix()
  
  ## Using R function solve to invert the matrix
  inversedMatrix <- solve(matrixBeforeInverse, ...)
  
  ## Set the inverted matrix
  x$setInversedMatrix(inversedMatrix)
  
  ## returns inversed matrix
  inversedMatrix  

}


