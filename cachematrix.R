## makeCacheMatrix and cacheSolve functions handle special "matrix" objects 
## which can cache the result of their inverse calculation

## Creates a "matrix" object out of matrix 'x', which is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
       
       ## set default value of the inverse to NULL
       inv <- NULL
       
       ## function to update the contained matrix
       ## resets inv value to default (matrix has changed)
       set <- function(y){
              x <<- y
              inv <<- NULL
       }      
       
       ## function to return the contained matrix
       get <- function(){
              x
       }
       
       ## function to update the inverse 
       setinv <- function(inverse) {
              inv <<- inverse
       }
       
       ## function to return the inverse
       getinv <- function(){ 
              inv
       }
       
       ## return the list of functions
       list(set = set,
            get = get,
            setinv = setinv,
            getinv = getinv)
} 


## Returns a matrix that is the inverse of the special matrix object 'x' created by makeCacheMatrix function
## If the inverse is already cached, return the cached value
## Otherwise, calculate the inverse and return it
cacheSolve <- function(x, ...) {
       
       ## get the cached value (already calculated/cached inverse or NULL)
       inv <- x$getinv()
       
       ## if inv not NULL, the result is already cached -> return it
       if(!is.null(inv)){
              message("Getting cached inverse")
              return(inv)
       }
       
       ## no cached result -> calculate the inverse, update the cache, and return the result
       message("No cache - calculating the inverse")
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setinv(inv)
       inv
}
