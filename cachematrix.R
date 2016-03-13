## makeCacheMatrix sets up the attributes and methods for
## a matrix caching object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    m <<- inv
  }
  getinv <- function() {
    m
  }
  
  #    label     function
  list(set    = set,
       get    = get,
       setinv = setinv,
       getinv = getinv)
}


## if an inverted matrix already exists, cacheSolve returns it
## if no inverted matrix exists, cacheSolve inverts it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  
  m
}

## driver code to test functions

orig_mat <- matrix(rnorm(25), 5, 5)

a <- makeCacheMatrix(orig_mat)

cacheSolve(a)
