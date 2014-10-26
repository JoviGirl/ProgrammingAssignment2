## Functions to cache a matrix and return its inverse


## Function to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
    iv = NULL   
    set <- function(matrix){
      x <<- matrix
      iv <<- NULL
  }
  get <- function(){
      x
  }
  setInverse <- function(inverse) {
    iv <<- inverse
  }
  getInverse <- function(){
    iv
  } 
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to return the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  
  m
}
