## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting matrix cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

mat <- matrix(c(1,1,4,0,3,1,4,4,0), nrow=3, ncol=3)
cmat <- makeCacheMatrix(mat)
cacheSolve(cmat)
cacheSolve(cmat)