## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initializes the inverse as nothing
  inv <- NULL
  
  # Sets the matrix (m) and the inverse as unset
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  # Getter to return the matrix itself
  get <- function() {
    m
  }
  
  # Setter to set the inverse as the parameter
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter to return the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Makes a matrix to return (pythonic habit)
  newList <- list(set = set, get = get, setInverse = setInverse,
                  getInverse = getInverse)

  return(newList)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Gets the inverse
  m <- x$getInverse()
  
  if (!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  
  data <- x$get
  # Solves the inverse and sets the inverse
  m <- solve(data) %*% data
  x$setInverse(m)
  
  # returns the matrix
  m
  
}
