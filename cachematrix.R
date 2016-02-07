#This functions allow time saving on inversing a certin matrix 
#by means of storing its cached inverse data  and return it whenever needed

#function makeCacheMatrix create cached data of a matrix and it's inverse  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
  
}


#function cachesolve check if a matrix inverse data already cached, 
#if so, it will skip the inverse calculation and directly return inversed matrix from the cache
#if it's not, it will calculate the matrix inverse and cache the inverse result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
