

## this funtion compute the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    ## <<- is used to assign the value to variable which is not in current scope
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse )
}


#this function returns a cached matrix that is inverse of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

# make a  matrix of containing 1,2,3,4 of dimensions 2*2
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
