## Functions help to store cache of inverse matrix 
## and to get it fast from the cache if it was calculated earlier

## This first function creates an auxiliary list for setting and getting cache
## This function should be called first

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set,get=get,setinv = setinv,getinv = getinv)
}

## This second function returns inverse matrix from the cache or calculates it
## Second function should be called with the variable, calculated from first function
## It returns "from cache" while taking inverse matrix from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    return(i)
    message("from cache")
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
