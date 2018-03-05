## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function create a special "matrix", a list object, that has as members:
##[[1]] a function "set" that stores  the value of a new matrix in the parent enviroment and assign the matrix 1x1 with NA value to the  object "matrix_inv_cached"
##[[2]] a function "get" that provides the value of the matrix
##[[3]] a function "setmatrix_inv" that stores the calculated inverse of the matrix  in "matrix_inv_cached" object
##[[4]] a function "getmatrix_inv" that that provides the calculated inversve of the matrix 



makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inv_cached <- matrix (NA,1,1)
  
  set <- function (y = matrix(0,1,1)) {
          x <<- y
          matrix_inv <<- matrix_inv_cached
        
  }
  get <- function() x
  setmatrix_inv <- function(matrix_inv_calculated) matrix_inv_cached <<- matrix_inv_calculated
  getmatrix_inv <- function() matrix_inv_cached
  list(set = set, get = get,
       setmatrix_inv = setmatrix_inv,
       getmatrix_inv = getmatrix_inv)
  
  
}


## Write a short comment describing this function

## The function provide the inverse of the special "matrix" object (created with the makeCacheMatrix function ) 
##                                  if it has NOT already been calculated and stored in the cache,
##                                  or  retrieve the inverse of the special "matrix" object
##                                  if it has already been calculated and stored in the cache,
##                                  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrix_inv_cached <- x$getmatrix_inv()
  if(!identical(matrix_inv_cached,matrix (NA,1,1))) {
    message("getting cached data")
    return(matrix_inv_cached)
  }
  data <- x$get()
  matrix_inv_cached <- solve(data, ...)
  x$setmatrix_inv(matrix_inv_cached)
  matrix_inv_cached
  
}
