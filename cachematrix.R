makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i<<- inverse
  getInverse <- function() i
  list(set = set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

cacheSolve <- function(x, ...){
  i <- x$getInverse()
  if(!is.null(i)){
    return(i)
  }
  m <- x$get()
  i<- solve(m, ...)
  x$setInverse(i)
  i
}

dmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
dmatrix$get()
dmatrix$getInverse()
cacheSolve(dmatrix)
kmatrix <- makeCacheMatrix(matrix(c(2, 6, 9, 4), 2, 2))
kmatrix$get()
kmatrix$getInverse()
cacheSolve(kmatrix)
