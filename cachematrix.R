## The two functions below creates a special matrix object that
## caches its inverse and computes the inverse of the special
## matrix.

## The first function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the value of the matrix inverse to NULL
    matinverse <- NULL
    ## Defining function set where the value be cached in
    set <- function(y) {
      x <<- y ## caches the inputted matrix
      matinverse <<- NULL ## sets the value of the matrix inverse to NULL
    }
    ## get function returns  the inputted matrix
    get <- function() x
    ## setinverse function set the inverse of the inputted matrix
    setinverse <- function(solve) matinverse <<- solve
    ## getinverse function returns the inverse of the inputted matrix
    getinverse <- function() matinverse
    ## returns the above functions for makeCacheMatrix
    list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)

}


## The function below computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.If the inverse has already 
##been calculated(and the matrix has not changed), then the 
#function below will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
     
    ## declared minverse variable 
      minverse <- x$getinverse()
    ## if the inverse is already in cache and the matrix has not 
    ## changed it gets it
      if (!is.null(minverse) & identical(minverse,solve(x$get(), ...))) {
        message("getting cached matrix")
        return(minverse) 
      }
    
    ## if the inverse is not in cache, it calculates it
    ## then sets it and return the inverse.
        data <- x$get()
        x$set(data)
        minverse <- solve(data, ...)
        x$setinverse(minverse)
        minverse
  
}
