#Note - For this assignment it is assumed that the matrix supplied is always invertible. 

 # This function creates a special "matrix" object that can cache its inverse
 # 1. set the value of the matrix 
 # 2. get the value of the matrix 
 # 3. set the value of inverse of the matrix 
 # 4. get the value of inverse of the matrix 

 makeCacheMatrix <- function(x = matrix()) { 
     i <- NULL 
     set <- function(y) { 
         x <<- y 
         i <<- NULL 
     } 
     get <- function() x 
     setinverse <- function(inverse) i <<- inverse 
     getinverse <- function() i 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 } 

 # This function computes the inverse of the special "matrix" returned by 
 # makeCacheMatrix above. If the inverse has already been calculated (and 
 # the matrix has not changed), then the cachesolve should retrieve the 
 # inverse from the cache.
 

 cacheSolve <- function(x, ...) { 
     i <- x$getinverse() 
     if(!is.null(i)) { 
         message("Getting cached data.") 
         return(i) 
     } 
     data <- x$get() 
     i <- solve(data) #solve function is used to calculate inverse of a square matrix
     x$setinverse(i) 
     i
 } 
  
