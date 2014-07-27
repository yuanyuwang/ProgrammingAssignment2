
########################################################################################################################
#makeCacheMatrix would return a list of functions to get/set the cache of inverse matrix and matrix so we do not need to
#calculate the inverse matrix multiple time. Then we can use cacheSolve to get the value of inverse matrix. If the inverse
#of matrix is previous calculated. It would return the cached version
#simple Usage
#c <-matrix(c(1,4,3,1),nrow=2,ncol=2)
#h=makeCacheMatrix(c) 
#cacheSolve(h)
#cacheSolve(h) if called 2nd time, will return the cached version without re-caculating. 

#makeCacheMatrix: return a list of function
#set: set the value of matrix
#get: get the value of matrix
#setInverseMatrix:set the value of inverse matrix
#getInverseMatrix:get the value of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


# cacheSolve: it will compute the inverse of the function. If the inverse has been calculated, it would return the 
#             cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setInverseMatrix(m)
  m
}



