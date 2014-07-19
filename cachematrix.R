
##This program outputs inverse of a matrix and stores the result. 
##If the same matrix is given as input, then 
## program retrieves the stored value. 
##Else, it calculates the inverse and stores the result. Input matrix is assumed to be invertible.

## makeCacheMatrix converts input matrix to a list and stores its inverse.It returns a list.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function takes a list as input. If inverse matrix of the input has already been stored, 
##it retrieves the result. Else, it calculates the inverse and stores the result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
