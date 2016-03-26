## Programming Assigment 2, Cousera "R Programming" at John Hopkins University
## Sam Vennell 26/03/2016
##
## Functions to create a special "matrix" object than can cache its inverse and to calculate that (square) matrix's 
## inverse, drawing from the cache to reduce computation if possible
##
## NOTE: at this stage it is assumed that the square matrix is invertible.


## makeCacheMatrix(x = matrix())
##
## Create a special "matrix" object used to cache the matrix and its inverse to avoid unnecessary computation
## Returns a list of four functions which:
## (1) Set the value of the stored matrix
## (2) Get the value of the stored matrix
## (3) Set the value of the stored matrix inverse (NOTE: This function stores, but does not calculate the inverse)
## (4) Get the stored inverse value
##
makeCacheMatrix <- function(x = matrix()) {
  #Set inv to NULL so that m is searched for in the parent environment (due to R's lexical scoping)
  inv <- NULL
  
  #Define the four functions (described above)
  set <- function(y) {
    x<<-y
    inv<<-NULL #Set the stored inverse to NULL in case the stored matrix has changed
  }
  get<-function() x
  setinv<-function(newinv) inv<<-newinv
  getinv<-function() inv
  
  #Return a list of the four functions (described above)
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(x, ...)
##
## Return a matrix that is the inverse of the 'special' matrix created by the above function
## It will skip the (potentially costly) computation if the inverse is already stored in the cache
## and otherwise calculate the mean and store it in the cache for future use.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'special' matrix object 'x'
  
  storedinv<- x$getinv()
  
  #Return the cached matrix if it exists
  if (!is.null(storedinv)) {
    message('Returning cached data')
    return(storedinv)
  }
  
  #Otherwise caculate the inverse, cache it and return it
  storedmat<-x$get()
  calcinv<-solve(storedmat)
  x$setinv(calcinv)
  return(calcinv)
  
}