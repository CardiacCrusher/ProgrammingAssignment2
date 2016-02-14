## makeCacheMatrix contains 4 functions
## 1. Set the matrix (using a matrix in the argument)
## 2. Get the matrix (returns the matrix values to the call)
## 3. Set the inverse (passes the matrix to the cacheSolve function)
## 4. Gets the inverse 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Uses the solve() function to calculate the inverse of the matrix
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  matrix.data <- x$get()
  inv <- solve(matrix.data, ...)
  x$setinverse(inv)
  inv
}
