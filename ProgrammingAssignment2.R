makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(a){
    x <<- a
    f <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) f <<- inverse
  getInverse <- function() f 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  f <- x$getInverse()
  if(!is.null(f)){
    message("getting cached data")
    return(f)
  }
  mat <- x$get()
  f <- solve(mat,...)
  x$setInverse(f)
  f
}
## https://jlinvegas.wordpress.com/ (got this information form this site)
## very very very new to programming and was running late in schedule 
