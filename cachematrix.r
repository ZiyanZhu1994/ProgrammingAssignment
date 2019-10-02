makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  l <<- list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- l$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- l$get()
  m <- solve(x)
  l$setinverse(m)
  m
}
