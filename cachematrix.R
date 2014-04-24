## cachematrix.R
## April 23, 2014
## Steve Spitz for Coursera: R Programming
##
## These functions initialize a matrix and compute its inverse.
## Once computed, the inverse is stored in cache memory.
## If the matrix inverse is requested after it has been cached, 
##     the object in cache is retrieve.  This avoids the need to
##     recalculate the inverse.
##
## The function makeCacheMatrix initializes a CacheMatrix object.
## This object contains four methods and two properties: 
##     the matrix itself and its inverse.
##
## The function cacheSolve checks to see if the inverse of the 
## matrix has already been computed and stored.  If so, it is returned.
## Otherwise, it is computed (via solve()) and cached.



# This function creates an instance of a "CacheMatrix".
# The CacheMatrix has four methods: setm(), getm(), setminv(), and getminv()
# setm() records a square matrix; getm() retrieves the matrix;
# setminv() stores the inverse of the matrix; and getminv() retrieves the inverse
makeCacheMatrix <- function(x = matrix()) {

  if(!(is.matrix(x))) stop("Error: The input to makeCacheMatrix must be a matrix.\n")
  if(dim(x)[1] != dim(x)[2]) stop("Error: The input matrix must be square.\n")
  
  minv <- NULL
  
  setm <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  getm <- function() x
  
  setminv <- function(minv) minv <<- solve(x)
  
  getminv <- function() minv
  
  return(list(setm=setm, getm=getm, setminv=setminv, getminv=getminv))
}



## Return a matrix that is the inverse of 'x'
# This function returns the inverse of the input matrix.
# If the inverse has already been computed and cached, it pulls the inverse from memory.
cacheSolve <- function(x, ...) {
  
  # check for the inverse in cache
  minv <- x$getminv()
  if(!(is.null(minv))) {
    cat("Retrieving the matrix inverse from cache.\n") 
    return(minv)
  }
  # the inverse hasn't already been calculated and cached
  mymatrix <- x$getm()
  minv <- solve(mymatrix, ...)
  x$setminv(minv)
  return(minv)
}
