## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function to return a list of functions to handle matrix inverse operation
makeCacheMatrix <- function(x = matrix()) {
  
  inverse<- NULL
  
  set<- function(y)
        {
          x<<-y
          inverse<<-NULL
        }
  get<- function() x
  
  
  setInverse<- function(y) inverse<<-y
  
  getInverse<- function() inverse
  
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
          
        
## Write a short comment describing this function
## function to calaculate inverse of a matrix. Caches the ressult 
##and calculates it again only if the data is changed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv))
  {
    print("returning cached value")
    return(inv)
  }
  print("returning calculated value")
  mat<-x$get()
  inv<-solve(mat)
  x$setInverse(inv)
  return(inv)
  
}
