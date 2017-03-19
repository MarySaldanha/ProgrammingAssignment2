## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(in_mtrx = matrix()) {
  out_mtrx <- NULL
  inc <- NULL
  setData <- function (out_m){
    in_mtrx <<- out_m
    out_mtrx <<- NULL
    inc <<- out_m 
  }
  getData <- function() in_mtrx
  getInput <- function() inc
  setInverse <- function(slv) out_mtrx <<- slv
  getInverse <- function() out_mtrx
  list( setData = setData, 
        getData = getData, 
        setInverse = setInverse, 
        getInverse = getInverse,
        getInput = getInput)
}


## Write a short comment describing this function

cacheSolve <- function (mtrx=matrix(), ...) {
  out_mtrx <- mtrx$getInverse()
  if (!is.null(out_mtrx)){
    if ( identical(mtrx$getInput(),mtrx$getData()) ) {
      message("Getting cached matrix inverse")
      return (out_mtrx)
    }
  }
  mtrx_data <- mtrx$getData()
  out_mtrx <- solve(mtrx_data,...)
  mtrx$setInverse(out_mtrx)
  out_mtrx 
}
