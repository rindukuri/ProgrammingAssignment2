## Cachematrix.R is a program to calculate inverse of a matrix 
## the results are cached for repeated use. 
## During execution first cache is checked for the inverse of matrix 
## and only if the value doesn't exist it will re-calculate
## program contains the followng 2 functions:

## 1. makeCacheMatrix
## 2. cacheSolve

## function "makeCacheMatrix" takes an input matrix 
## using "<<-" the function creates a list for caching in current environment
## the list consists of the following:
## a. "set"
## b. "get"
## c. "setinverse" 
## d. "getinverse"

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function calculates the inverse of matrix using "solve"
## check if the inverse of matrix is cached and return from cache
## if not exist in cache, calculates inverse matrix and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setinverse(I)
  I
}

## example to test the makeCacheMatrix and cacheSolve functions
##
## > x = matrix(c(1,2,3,2,3,1,3,1,2), nrow=3, ncol=3, byrow=TRUE)
## > a = makeCacheMatrix(x)
## > names(a)
## [1] "set"        "get"        "setinverse" "getinverse"
## > a$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    2    3    1
## [3,]    3    1    2
## 
## Initial execution of cacheSolve function - without cached results
## > cacheSolve(a)
## [,1]        [,2]        [,3]
## [1,] -0.27777778  0.05555556  0.38888889
## [2,]  0.05555556  0.38888889 -0.27777778
## [3,]  0.38888889 -0.27777778  0.05555556
##
## Next Execution of cacheSolve function - with cached results 
## > cacheSolve(a)
## getting cached data
## [,1]        [,2]        [,3]
## [1,] -0.27777778  0.05555556  0.38888889
## [2,]  0.05555556  0.38888889 -0.27777778
## [3,]  0.38888889 -0.27777778  0.05555556
