## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates a special list that creates cached values for calculating inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invVal <- NULL 
  set <- function(y){
    x <<- y
    invVal <<- NULL
  }
  get <- function() x #get the original matrix entered
  setcache <- function(solve) invVal <<- solve(x) #sets variable invVal the inverse of matrix x
  getcache <- function() invVal #makes variable getcache the value of invVal, inverse of x
  list(get = get,
       set = set,
       setcache = setcache,
       getcache = getcache) #returns a list, with all values calulated from above.
}


## Write a short comment describing this function
#takes as input a matrix
#checks if it is cached
#if cached, return cached value
#if not cached, solve for inverse, cache that value then, return inverse

cacheSolve <- function(x=matrix(),...) {
        ## Return a matrix that is the inverse of 'x'
        
    invVal <<- x$getcache()#get cache from value outside current environment
    
    #check if inverse in cache
    if(!is.null(invVal)){
      message("Getting Cached Value")
      return(invVal)
    }
    dataMat <- x$get() #get matrix we want inverse of
    invVal <- solve(dataMat) #calculate inverse
    x$setcache(dataMat) #set cache to inverse
    invVal #return inverse
}
