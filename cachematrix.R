## makeCacheMatrix takes a specified matrix and turns it into a 
## matrix object whose inverse can be cached

## cacheSolve computes the inverse of the cached matrix and 
## returns the inversed matrix

## code for makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  init <- NULL # initial object
  setme <- function(y) {
    x <<- y
    init <<- NULL
  } # sets the value of matrix 'x'
  getit <- function() x # gets the value of matrix 'x'
  sinverse <- function(inverse) init <<- inverse # sets the inverse value of 'x'
  ginverse <- function() init # gets the inverse value of 'x'
  list(setme = setme,
       getit = getit,
       sinverse = sinverse,
       ginverse = ginverse)
}


## code for cacheSolve

cacheSolve <- function(x, ...) {
  init <- x$inverse()   ## Return a matrix that is the inverse of 'x'
  if (!is.null(init)) { # checks if the matrix object actually has data
    message("getting cached data")
    return(init)
  }
  data <- x$getit() # retrieves the cached matrix
  init <- solve(data, ...) 
  x$sinverse(init) # computes and returns the inverse values
  init  
}
