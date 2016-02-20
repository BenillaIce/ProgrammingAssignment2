## R programming course - Author: Antti Löytynoja, antti.loytynoja@gmail.com
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
   inv<-NULL
   
   set<-function(y) {
     
     x<<-y
     inv<<-NULL
   }
   get<-function() x
   setInv<-function(invVal) inv<<-invVal
   getInv<-function() inv
   list(set = set, get = get,
        setInv = setInv,
        getInv= getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  
  ## Check input first before calculating inverse
  if(nrow(data) != ncol(data)){ # must be square
    message("Matrix not square")
  } else if(det(data)==0) {
    ## matrix not singular if determinant != 0
    message("Matrix is singular. Cannot calculate.") 
  } else {
     inv<-solve(data)
     x$setInv(inv)
     inv
  }
  
}
