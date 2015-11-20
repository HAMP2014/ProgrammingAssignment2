
## Write a short comment describing this function
## creates object and will cache the inverse of the object 
##to save time in computations

makeCacheMatrix <- function(x = matrix()) {
inv <-NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv 
list (set = set
      , get = get
      , setInverse = setInverse
      , getInverse = getInverse
      )
}


##computation of the inverse of the above object(makeCacheMatrix)
#inverse, if created, will be pulled from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  return(inv)
  
}


 