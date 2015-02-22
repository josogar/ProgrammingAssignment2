## Set of functions for the creation of matrix objects and
## calculation of their inverse in a way that the result 
## of the inverse operation gets cached in the object the first time
## it is computed, and simply retrieved in subsequent times
 

## This function creates the special matrix object, providing a list
## of functions to 
## - get the value of the matrix 
## - set the value of the matrix
## - get the cached value of the inverse 
## - set the cached value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInv <- function(invM) invX <<- invM
  getInv <- function() invX
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function takes as input a matrix created with the previous
## function and returns its inverse.
## If the inverse is available in the cache it is simply retrieved 
## and returned. Otherwise it is computed with a call to solve, 
## and stored in the cache for subsequent use.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setInv(invX)
  invX
}
