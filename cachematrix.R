## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) { ## to reset matrix
    x <<- y ##assign matrix to x
    inver <<- NULL ##initialize the inverse to null
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("retrieving cached data")
    return(inver) ## if it's already been calculated return the original matrix
  }
  mat <- x$get() 
  inver <- solve(mat, ...) ##calculate inverse matrix
  x$setInverse(inver) ##assign the matrix
  inv ##print the matrix
}
