makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse (to be retrieved by
  # the function "cacheSolve")
  # The object created is a list which contains four functions which
  # 1- set the value of the matrix
  # 2- get the value of the matrix
  # 3- set the value of the inverse
  # 4- get the value of the inverse
  #
  # Args: a square matrix x.
  # Warning: the code does not check if the matrix is invertible. Proceed with caution.
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  # Computes the inverse of the special "matrix" created using the function "makeCacheMatrix".
  # If the inverse has already been calculated and the matrix has not changed, the the
  # inverse will be retrieved from the cache, thus saving time.
  # 
  # Args: a special "matrix" x created with the function "makeCacheMatrix"
  # Returns: the inverse of the special "matrix" x
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
