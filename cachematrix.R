## The basic idea is to compute the inverse of a matrix
## if the matrix's inverse is already found then the
## previously found inverse is returned 
## otherwise it is computed

## This function makes the cache matrix which is 
## the plave to find the earlier computed
## matrice's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function actually returns the computed/cached
## inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(! is.null(m)){
      message("getting cached data")
      return(m)
    }
    mat <- x$get()
    if( det(x) != 0){
      m <- solve(mat,...)
      x$setInverse(m)
      m
    }
}
