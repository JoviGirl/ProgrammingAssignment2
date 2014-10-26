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
  setinverse <- function(inverse) {
    iv <<- inverse
  }
  getinverse <- function(){
    iv
  } 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to return the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  
  m
}
