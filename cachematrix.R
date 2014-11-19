## Functions implement an matrix-object capable of caching its inverse value
## usage: 
##    create a new object:                  ematrix<-makeCacheMatrix(numeric_matrix)
##    extract numeric matric:               ematrix$get()
##    change the matrix value:              ematrix$set(new_numeric_matrix)
##    calculate the numeric inverse matrix: cacheSolve(ematrix)

## Creates a matrix-object enabling to hold its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)) 
    {
      stop("the argument is not a matrix")
    }
  
  invM <- NULL
  #the function sets the object value
  #usage: obj$set(matrix(...))
  set <- function(y) {
    if(!is.matrix(y)) 
    {
      stop("the argument is not a matrix")
    }
    x <<- y
    invM <<- NULL
  }
  #the function extracts the numeric matrix
  get <- function() x
  
  #the internal function used to store inverse matrix in parent environment
  setinv <- function(invm) invM <<- invm
  #the internal function used to recall inverse matrix from  parent environment
  getinv <- function() invM
  #the object is represented as a list of handler functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calcurate the matrix inverse if it hasn't been computed yet or restore it from its cache

cacheSolve <- function(x, ...) {
  
  invM <- x$getinv()
  if(!is.null(invM)) {
    #return inverse from cache
    return(invM)
  }
  
  data <- x$get()
  ndim=dim(data)
  if(ndim[1]!=ndim[2])
  {
    stop("the  matrix is not square")
  }
  invM <- solve(data,...)
  x$setinv(invM)
  ## Return a matrix that is the inverse of 'x'
  invM
}
