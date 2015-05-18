## These functions are used to create a matrix, store the matrix in the cache memory and calculate the inverse of the matrix

## This function creates a matrix and calculates the inverse of this matrix
makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x

  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## this function checks if the inverse matrix is already cached and if not calculates and stores the inverse matrix
cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}