## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y){
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix)  mtx <<- solveMatrix
  getInverse <- function() mtx
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtx <- x$getInverse()
  if(!is.null(mtx)){
    message("Getting Cached Data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data,...)
  x$setInverse(mtx)
  mtx     
}


