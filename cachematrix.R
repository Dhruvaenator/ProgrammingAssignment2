## makeCacheMatrix creates custom matrix type capable of running functions

## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ 
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <-function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  I <- x$getInverse() ## If the inverse has already been calculated (and the 
                      ## matrix has not changed), then it should retrieve the inverse from the cache.
  if(!is.null(I)) {  
    return(I)
  }
  
  data <- x$get()   ## otherwise it will calculate the inverse of the matrix
  I <- solve(data) 
  x$setInverse(I)  
  I               
 
}
