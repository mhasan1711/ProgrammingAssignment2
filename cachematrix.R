## makeCacheMatrix
## has 1 required parameter and is defaulted to a 1 X 1 matrix with no value

makeCacheMatrix <- function(x = matrix()) {
  
  varInverse <- NULL # initialize it to NULL
  set <- function(y) {
    x <<- y  # set it in parent env
    varInverse <<- NULL # reset varInverse to NULL if the matrix changes
  }
  get <- function() x # gets the value of the initial matrix
  setInverse <- function(inverse) varInverse <<- inverse # sets the value in parent env
  getInverse <- function() varInverse  # getter function for varInverse
  # needed to be used by cacheSolve param; to avoid object of type 'closure' is not subsettable error
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
  
}


#CacheSolve function retuns the inverse of a matrix or spits out a relevant message

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() ## gets the value from the varInverse variable and assigns it to variable m
  # if m is not NULL returns the value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # gets the initial matrix assigned using the set function and stores in a variable data
  data <- x$get()
  # check is the matrix is a square matrix
  if(nrow(data) != ncol(data))
  {
    m <- message("Matrix is not a square matrix")
    x$setInverse(m)
  }
  # if det(matrix) !0 tells us that an inverse of a matrix exists
  else if(det(data) != 0)
  {
    m <- solve(data, ...)
    x$setInverse(m)
  }
  # generic message for everything else
  else
  {
    m <- message("Inverse not possible")
    x$setInverse(m)
  }
  
  m
  
  
  
}