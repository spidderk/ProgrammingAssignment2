## The high level objective of the following code is to demonstrate
## the concepts of lexical scoping and object oriented programming in R.

## These functions are used to cache the computed inverse of a matrix.
## Since computing the inverse of a matrix is an expensive operation,
## it makes sense to cache the result if the matrix has not change
## since the inverse was last calculated.

## makeCacheMatrix() takes as input a matrix and returns a list of 
## named functions which can be used to get/set the value and inverse 
## of an object of type makeCacheMatrix.
## Since these are named functions, they  may be accessed using the 
## $ operator, 
## Example usage:
## > myRegMatrix <- matrix(1:4, ncol=2, nrow=2)
## > myCacheMatrix <- makeCacheMatrix()  # creates an object with value NULL
## > myCacheMatrix$set(myRegMatrix)      # sets the value of myCacheMatrix
## > myCacheMatrix$get()                 # gets the value of myCacheMatrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## etc.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Define the get/set operators for the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  ## Define the get/set operators for the matrix inverse
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv

  ## Return a named list of the get/set functions.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() takes as input an object of type makeCacheMatrix and
## returns the inverse of the input. The inverse is retrieved from
## the stored value within the object if it has been previously
## calculated, else it calls the solve() function to compute the 
## inverse of the matrix.
## Example usage:
## > myRegMatrix <- matrix(1:4, ncol=2, nrow=2)
## > myCacheMatrix <- makeCacheMatrix()  # creates an object with value NULL
## > myCacheMatrix$set(myRegMatrix)      # sets the value of myCacheMatrix
## > cacheSolve(myCacheMatrix)           # 1st time: calculates inverse using solve
## > cacheSolve(myCacheMatrix)           # 2nd time: retrieves the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## First check if input is of type makeCacheMatrix (i.e. it is a list
  ## with a $getinv function). If not, return a helpful message.
  if (!with(as.list(x), exists("getinv"))) {
    return(message("cacheSolve requires input of type makeCacheMatrix"))
  }

  ## Next get the inverse value from parent object.
  inv <- x$getinv()

  ## If inverse is not NULL, return the value retrieved from parent. 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## Else calculate the inverse using solve(), set the inverse in the 
  ## parent object, and return the calculated inverse value.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
