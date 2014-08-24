## makeCacheMatrix function creates a special object (namespace),
## that contains the passed matrix, the inverted matrix and interface 
## functions to access them

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function provides an inverse of a matrix object,
## created with makeCacheMatrix function, either by computing it or
## using previos computation, stored in makeCacheMatrix object 
## (if possible)

cacheSolve <- function(x, ...) 
{

if(!is.null(inv)) 
  {
    return(inv)
  }
  
  data <- x$get()

  if(det(data))
    inv <- solve(data, ...)
  else
    inv <- NaN
  
  x$setinv(inv)
  inv
}
