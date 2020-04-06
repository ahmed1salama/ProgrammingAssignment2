
## This function creates a special[getMatrix-setMatrix-getInverse-setInverse] "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # initializing inverse "i" with NULL
  i <- NULL
  
  #assign value of the matrix given y to the special x 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # return the matrix data
  get <- function() x
  
  # sets the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  # returns the inverse of the matrix
  getinverse <- function() i
  
  # return list of functions associated with the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" directly if cached, or compute and return it if not
cacheSolve <- function(x, ...) {
  # get the current value of the inverse
  i <- x$getinverse()
  
  # check if the inverse is not null(already calculated) --> return its already calculated value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if the inverse is not yet calculated --> get the matrix data --> compute its inverse --> then save the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## for testing uncomment the R code parts ##

# initialize the special matrix
#matr = makeCacheMatrix(x)

# set its data
#matr$set(matrix(1:4, nrow = 2, ncol = 2))

# try to get the inverse 
#matr$getinverse() # returns null for the first time

# compute the inverse
#cacheSolve(matr) # returned value is computed from scratch
#cacheSolve(matr) # return value is the cached inverse 
