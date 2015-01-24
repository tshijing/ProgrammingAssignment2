## These functions are mean to cache the inverse of a matrix.

## The first function, `makeCachelMatrix` creates a special "matrix", which is
## really a matrix containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inv
## 4.  get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # return the input matrix
  setinv <- function(inv) m <<- inv # set the inversed matrix
  getinv <- function() m # return the inversed matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # return a list containing the above 4
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinv() # get the inversed matrix from x
  if(!is.null(m)) { # if the inversed matrix from x already existes
    message("getting cached data") # print "getting cached data"
    return(m) # return the inversion
  }
  data <- x$get() # if the inversed matrix is not there
  m <- solve(data, ...) # use solve function to get the inversion
  x$setinv(m) #set it to the object
  m        ## Return a matrix that is the inverse of 'x'
} 