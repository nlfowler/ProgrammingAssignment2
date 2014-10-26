#function creation line
makematrix <- function(x = matrix()) {
  #null variable
  m <- NULL
  #set function variables
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #solve for the inverse
  setinverse <- function(solve) m <<- solve
  #inverse function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("get inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
