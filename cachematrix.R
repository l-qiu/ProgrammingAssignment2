#-------------------------------------------------------------------------#
# some calculation may take long time and it is beneficial not to repeat 
# such costly caluculation unnecessarily. One way to avoid this is to 
# retreive values calculated previously from cache. 
#-------------------------------------------------------------------------#

#-------------------------------------------------------------------------#
# Matrix inverse calculation is usually a costly. In this assignment I, 
# the two template functions provided in the assigment itself.
# the 1st funciton (MakeCacheMatrix) 
#     a:) create a matrix and place it in global environment retriveable  
#         by the 2nd function (use special operator <<-)
#     b:) calculate the inverse

# the 2nd function (CacheSolve) 
#     a:) check if the inverse of the matrix from 1st function exist first
#     b:) Retrieve the inverse from cache if it has been calculated already 
#         and there is no change of the orginal matrix itself 
#     c:) calucate the inverse if it does not exist in the cache
#-------------------------------------------------------------------------#
MakeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y           ## set the value of matrix           
    inv_x <<- NULL        
  }
  
  get <- function() {x} ## get the value of matrix
  
  setSolve <- function(solve) {inv_x <<- solve}  ## set the value of inverse of matrix
  getSolve <- function() {inv_x}                 ## get the value of inverse of matrix
  ls1<<- list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}



CacheSolve <- function(x, ...) {
  inv_x <- x$getSolve()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setSolve(inv_x)
  inv_x   ## Return a matrix that is the inverse of 'x'
}