## creates a matrix that the inverse can be stored for efficient calculation purposes

## this function creates a matrix that the inverse is cache-able

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function returns the cached inverse matrix if it already exists; otherwise 
## it calculates it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  }
