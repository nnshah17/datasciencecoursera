## Following functions calculate inverse of a matrix and cache the result

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){

  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) invMatrix <<- solve
  getInvMatrix <- function() invMatrix
  
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## This function returns inverse of a matrix. If the inverse of matrix is already calculated, it is returned from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matSolve <- x$getInvMatrix()
  if(!is.null(matSolve)){
    message("getting cached data")
    return(matSolve)
  }
  data <- x$get()
  matSolve <- solve(data)
  x$setInvMatrix(matSolve)
  matSolve
}
