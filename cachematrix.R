## Two functions that create, store, and calculate the mean for a "special" matrix
## 

## Create a special matrix object that is able to cache it's matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setmatrix <- function(solve) m <<-solve
  getmatrix <- function () m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the cached matrix from the function above

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
