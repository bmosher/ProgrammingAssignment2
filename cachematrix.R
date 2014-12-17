## Together these functions provide a meachanism for solving for and
## caching the inverse value of a matrix.

## makeCacheMatrix accepts a matrix as an argument with:
##   get and set functions to get and set the matrix
##   get and set functions to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  # The cached inverse value
  inverse <- NULL

  set <- function(newMatrix) {
    # set the new matrix value
    x <<- newMatrix
    # and reset the old inverse value, since it is no longer valid
    inverse <<- NULL
  }

  get <- function() {
    # return the matrix
    x
  }

  setInverse <- function(newInverse) {
    # set the new inverse value
    inverse <<- newInverse
  }
  getInverse <- function() {
    # return the cached inverse value
    inverse
  }

  # return a list of functions
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## cacheSolve accepts a matrix created with makeCacheSolve
## and returns the inverse of the matrix
##
## If the inverse is cached
##    it returns the cached value
## Otherwise
##    it solves for the inverse
##    stores the value in cache
##    and returns the inverse value

cacheSolve <- function(x, ...) {
  # get the cached inverse value
  inverse <- x$getInverse()

  # if the value is set (not null)
  if(!is.null(inverse)) {
    # return the cached value
    inverse
  }

  # otherwise get the matrix data
  data <- x$get()
  # solve for the inverse
  inverse <- solve(data)
  # cache the inverse
  x$setInverse(inverse)
  # return the inverse
  inverse
}
