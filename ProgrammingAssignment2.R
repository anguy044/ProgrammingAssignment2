## This function takes in a matrix of any size inputed by the user,
## and creates four functions to cache information for the matrix. 
## The functions are then returned in a list format. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## create a variable to store the value of the matrix x's inverse
  set <- function(y) {  ## function setting the matrix the value y and setting the inverse value to NULL
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## function containing the matrix
  setinverse <- function(inverse) m <<- inverse  ## function setting the inverse of value for m to the inputed value, inverse. 
  getinverse <- function() m  ## function containing the value of the matrix's inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## Returning a list of the functions created.
  
}

## This function takes in a matrix x and finds the value of the inverse of x. 
## If the value of the inverse is already discovered, the function will 
## return a message and the inverse without the need to calculate the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()  ## assigning the variable m the value of the matrix inverse
  if(!is.null(m)) {  ## if function to see if the inverse for matrix x is already set. 
    message("getting cached inverse")  ## if set, will return message and inverse. 
    return(m)
  }
  data <- x$get()  ## if inverse not previously set, will solve inverse of the matrix x and return its inverse. 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
