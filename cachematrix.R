##  caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<- function()x
  setinv <- function(solve) inv<<- solve
  getinv <- function() inv
  list(set = set, get= get, setinv= setinv, getinv = getinv)

}


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

## At the moment giving: Error in x$getinv : $ operator is invalid for atomic vectors
cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  return (inv)
}
        

