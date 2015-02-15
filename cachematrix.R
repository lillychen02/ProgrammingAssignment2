## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m << y
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special matrix
#If the inverse has already been calculated and the matrix has not changed,
#then cacheSolve will return the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matr <- x$get()
    inver <- x$getinverse
    #check to see if matr == x, and if inverse exists
    if (matr == x && !is.null(inver)) {
      return inver
    } 
     #if not, then calculate inverse, then set inverse
      inver <- solve(matr)
      x$setinverse(inver)
      return inver
   
}
