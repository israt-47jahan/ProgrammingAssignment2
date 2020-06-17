makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  j <- NULL
  set <- function(y){
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  # if the inverse has already been calculated
  if(!is.null(j)){
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(j)
  }
  
  # otherwise, calculates the inverse 
  mat <- x$get()
  j <- solve(mat,...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(j)
  return(j)
}
  
