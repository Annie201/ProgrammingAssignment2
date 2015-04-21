## Making special obejct with cached inversed matrix of input x, and
## recomputing the inversed matrix when cached value is null.
##
## how to execute
## test <- makeCacheMatrix(matrix x)
## cacheSolve(test)

## A function to creat a special object with matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                  ##setting cache as NULL for inverse matrix
  set <- function(y) {                       ##defining function of matrix x and i
    x <<- y
    i <<- NULL
  }
  get <- function() x                        ##returning x      
  setinverse <- function(solve) i <<- solve  ##setting inversed x as i
  getinverse <- function() i                 ##rerutning inversed x as i
  list(set = set, get = get,                 ##returning special list
       setinverse = setinverse,              ##with functions 
       getinverse = getinverse)
}


## A function to generate special "inverse matrix" 
## with makeCacheMatrix function.
## Checking cached inversed matrix before a new computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()                 ##getting i as inversed x from cache memory
  if(!is.null(i)) {                   ##checking chached inversed matrix
    message("getting cached data")
    return(i)
  }
  input_data <- x$get()               ##with empty cached, getting x
  i <- solve(input_data, ...)         ##inverse x
  x$setinverse(i)                     ##set inversed matrix of x as i
  i                                   ##return inversed matrix i
}
