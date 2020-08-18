## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Output of makeCacheMatrix()
## Return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)){
    # get it from the cache and skips the computation.     
        message("getting cached data")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  # sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(m)
}
