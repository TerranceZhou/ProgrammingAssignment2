## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix set and get the square martix and its inverse martix

makeCacheMatrix <- function(x = matrix()){
  
  ## test if a square martix inputed
  
  try(if (nrow(x)!=ncol(x)) stop("square matrix only"))
  m <- NULL
  
  ## set martix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get martix
  get <- function() x
  
  ##set inverse martix
  setinverse <- function(inverse) m <<- inverse
  ##get inverse martix
  getinverse <- function() m
  
  ## return the result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}


## cacheSolve checks if there is a memory result, if yes, get from there
## if not, use makeCacheMatrix to build one

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
       
 ## Return a matrix that is the inverse of 'x'
}



