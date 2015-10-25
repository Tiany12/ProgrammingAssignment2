## 2015-10-25 Beijing All copyright reserved by Field
## Here we have 2 functions to manipulate the matrix in order to calculate the inverse faster
## And the algorithm is just similiar as the example with vector, so here I just made some modifications

## "makeCacheMatrix" function will return a list contains the value, inverse of the matrix, namely get() & getInverse()
## btw, the set() and setInverse() are doing the calculating

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
       set<- function(y){
             x<<- y
             m<<- NULL
        }
       get<- function() x
       setInverse<- function() m<<- solve(x)
       getInverse<- function() m
       list ( set=set,get=get,setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" will calculate the inverse of a matrix processed by "makeCacheMatrix", if the inverse was cached, just retrieve it.

cacheSolve <- function(x, ...) {
        m<- x$getInverse()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
          }
        data <- x$get()
        m<- solve(data,...)
        x$setInverse()
        m
}
