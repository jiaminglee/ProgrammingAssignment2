## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list with a function
# function will set and get value of matrix
# function will set and get valye of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  #get and set method
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# The cacheSolve function returns the inverse of matrix 
# Function will check for the inverse to see if it has already been computed
# Function will return the results if computed
# Function will computes the inverse if not computed and set the value with setinverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## start example of funtion with results
## x = rbind(c(1, -2), c(-2, 1))
## m = makeCacheMatrix(x)
## m$get()
## [,1] [,2]
## [1,]    1   -2
## [2,]   -2    1
## cacheSolve(m)
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333
## cacheSolve(m)
## getting cached data
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333
## end example