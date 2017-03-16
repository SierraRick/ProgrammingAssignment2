## These two functions make a cache to store the
## inverse of a matrix, and also solve the matrix
## if it has not already been solved.  Or, it retrieves
## the matrix from the Cache of it has been stored
## previously. 

## makeCacheMatrix is a function that creates a list
## of functions.  
## set will "set" the original matrix data
## get will "get" the matrix data
## setinverse will "set" the inverse of the original matrix into the cache
## getinverse will retrieve the inverse of the original matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  
   m <- NULL
   set <- function(y) {
      x<<-y
      m<<-NULL
   }
  
  get <- function() x
  
  setinverse <- function(matrixinverse) m <<- matrixinverse
  getinverse <- function() m
  
  list (set = set,get = get,
        setinverse=setinverse,
        getinverse=getinverse)
  
}


## cacheSolve functions either
## 1) Computes the inverse of the matrix passed to it in 'x'
## 2) or it uses the existing inverse matrix from cache
##    that was retrieved with the 'getinverse' function.
## 3) if the matrix inverse was calculated, then this function
##    will 'setinverse' put the calculated inverse matrix 
##    into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m  = x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <-x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m

  }

