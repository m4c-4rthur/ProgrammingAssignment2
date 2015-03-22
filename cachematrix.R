## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates custom matrix type which has four functions like the following :-
## 1- set: which stores the matrix in cache
## 2- get: which recalls the matrix
## 3- setInverse : same as set but it inverse the original matrix
## 4- getInverse : same as get but it inverse the original matrix

makeCacheMatrix <- function(x = matrix()) {   
      i <- NULL            
      set <- function(y){    #set matrix
      x <<- y  
      i <<- NULL    #store matrix in cache 
    }
    get <- function() x   #get matrix
    setInverse <- function(solve) i<<- solve #set matrix inverse
    getInverse <- function() i #get matrix inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   # create list of the functions
  }

## Write a short comment describing this function
## cacheSolve take a custom matrix type which was initialized by the makeCacheMatrix function
## Then it's calculate the inverse matrix of it
## cacheSolve checks first to indicate if the calculation has been done before or not
## if the condition is true it retrieves the data from the cache variable i. If it has not been done 
## before it calculates the inverse matrix then store it in the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getInverse() #if there is a cache the inverse has been previously calculated               
  if(!is.null(i)){     
    message("retrieving cached version")
    return(i)                         
  }
  data <- x$get()             # get the matrix used by makeCacheMatrix function                 
  i <- solve(data, ...)       # calculate the inverse of the matrix        
  x$setInverse(i)             # store the inverse matrix in cache using the makeCacheMatrix set function
}
