## These two functions are able to cache the potentially time-consuming computation of
## inverse of a matrix; this caching action is useful when it needs to calculate 
## the inverse of the same matrix multiple times, for example in a loop.

## The first function creates a special matrix, which is really a list containing a function to
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function calculates the inverse of the special matrix 
## created with the first function. It checks if the inverse has already been calculated;
## if it's true it takes the cached inverse (via getsolve), otherwise it calculates the inverse
## and set the value in the cache (via setsolve).

cacheSolve <- function(x, ...) {
  ## gets the cached inverse
  m <- x$getsolve()
  ## if the inverse is not empty it returns the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else it takes the matrix...
  data <- x$get()
  ## ...calculates the inverse...
  m <- solve(data, ...)
  ## ...and caches the value
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
