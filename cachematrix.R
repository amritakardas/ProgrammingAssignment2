## 'makeCacheMatrix' and 'cacheSolve' are a pair of functions that allow
## caching the inverse of a matrix to avoid calculating it more than once.
##
## Sample usage:
# a= makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2))
# > a$get()
#       [,1] [,2]
# [1,]   10   30
# [2,]   20   40
# cacheSolve(a)
#      [,1]  [,2]
# [1,] -0.2  0.15
# [2,]  0.1 -0.05
# a$getinverse()  # Returns matrix inverse
#      [,1]  [,2]
# [1,] -0.2  0.15
# [2,]  0.1 -0.05
# a$set(matrix(c(0,50,90,60), nrow=2, ncol=2)) # Modify existing matrix
# > cacheSolve(a)   # Computes, caches, and returns new matrix inverse
#             [,1] [,2]
# [1,] -0.01333333 0.02
# [2,]  0.01111111 0.00
# a$get()         # Returns matrix
#       [,1] [,2]
# [1,]    0   90
# [2,]   50   60
# a$getinverse()  # Returns matrix inverse
#             [,1] [,2]
# [1,] -0.01333333 0.02
# [2,]  0.01111111 0.00

## makeCacheMatrix: used to create a matrix with cachable inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initial the inverse matrix of x with NULL
  inverse <- NULL
  ## set function: assign y to x and NULL to inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get function: return x
  get <- function() x
  ## setinverse/getinverse: used to set and get 
  ## cached inverse matrix, accordingly.
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}
cacheSolve <- function(x, ...) {
  ## getinverse from matrix x
  inverse <- x$getinverse()
  
  ## if inverse was ever computed, return the existing one.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## in case no cached inversed matrix, do the following:
  ## get matrix x
  m <- x$get()
  inverse <- solve(m, ...)
  x$setinverse(inverse)
  inverse
}
