## These are functions that manipulate matrix related operations. 

## Sets, gets, sets the inverse, and gets the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(value = matrix()){
    x <<- value
    inverse <<- NULL
  }
  
  get <- function(){
    x
  }
  
  set_inverse <- function(inversed){
    inverse <<- inversed
  }
  
  get_inverse <- function(){
    inverse
  }
  
  list(
    set = set
    ,
    get = get
    ,
    set_inverse = set_inverse
    ,
    get_inverse = get_inverse)
}


## Returns the inverse of the given makeCacheMatrix if cached otherwise inverses 
## it and caches the inverse for later us. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  
  if (is.null(inverse)) {
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
  }
  else{
    message("Getting cached data.")
  }
  inverse
}
