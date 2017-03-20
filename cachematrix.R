## Caching the Inverse of a Matrix 
## Coursera R Assignment Week 3, by IG

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly . 
## Below are a pair of functions to be used to cache the inverse of a matrix.

## This fuction creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x =  matrix()){      
  inm <- NULL                                    ## initiate inm as NULL
  set <- function(y){                            ## set the value of the matrix
    x   <<- y
    inm <<- NULL
  }
  get <- function() x                            ## get the value of the matrix
  setinverse  <- function(inverse)inm<<-inverse  ## set the value of the inverse of the matrix
  getinverse  <- function() inm                  ## get the value of the inverse of the matrix
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix" created with the above funcion.
cacheSolve <- function(x, ...){
  inm <- x$getinverse()
  if(!is.null(inm)) {                            ## check if the inverse has already been calculated
    message("getting cached data")           
    return(inm)                                  ## if so, get the inverse from the cache and skips the computation
  }
  data <- x$get()                  
  inm <- solve(data, ...)                        ## otherwise, calculate the inverse of the data and set the value in the cache via setinverse function
  x$setinverse(inm)
  inm
}

## Testing ##
matrix_test <- makeCacheMatrix(matrix(1:4,2,2))
matrix_test$get()
matrix_test$getinverse()
cacheSolve(matrix_test)
cacheSolve(matrix_test)
