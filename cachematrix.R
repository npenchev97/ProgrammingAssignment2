## Nikolay Penchev

## This function creates a new matrix whose inverse is cached in memory and is then printed.

makeCacheMatrix <- function(x = matrix()) 
{
  matrix <- NULL     ## matrix starts from scratch
  set <- function(y) 
  {
    x <<- y          ## assigned from different environment using <<-
    matrix <<- NULL  ## assigned from different environment using <<-
  }
  get <- function() x
  setInverse <- function(inverse) matrix <<- inverse
  getInverse <- function() matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ## gets values and caches the new matrix
}


## This function goes past the function above and actually computes the inverse of the cached matrix by using the Solve function. The inverse matrix is then printed.

cacheSolve <- function(x, ...) 
{
  matrix <- x$getInverse()
  if (!is.null(matrix))      ## Checks if matrix is not null
  {
    message("getting cached data")
    return(matrix)           ## Matrix is returned if it's noot null
  }
  mat <- x$get()
  matrix <- solve(mat, ...)  ## Solves inverse of the checked matrix
  x$setInverse(matrix)       ## Sets inverse matrix and is outputted
  matrix
}