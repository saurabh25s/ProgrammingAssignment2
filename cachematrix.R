## makeCacheMatrix() stores the cache of the matrix and cachesolve() calculates the inverse of the matrix


## makeCacheMatrix() function takes a matrix as an input and will return a list of 4 functions 
## the input matrix is placed in the getmatrix() funciton
## the cachesolve() function uses the "special matrix" list created by this makeCacheMatrix() and calculates the inverse
## the value of this inverse is stored in the cache for further use.

makeCacheMatrix <- function(splmatrix=matrix()){
  inverse<-NULL
  getmatrix<-function() splmatrix
  setmatrix<-function(newmatrix){
    splmatrix<<-newmatrix
    inverse<<-NULL
  }
  getinverse<-function() inverse
  setinverse<-function(newinverse) inverse<<-newinverse
  list(getmatrix=getmatrix,setmatrix=setmatrix,getinverse=getinverse,setinverse=setinverse)
  
}

## cachesolve() function has special "matrix" created in the makeCacheMatrix as its argument
## it calculates the inverse of the matrix passed as an argument to makeCacheMatrix
## the function first checks if the inverse has been calculated before and returns the cached inverse as the result
## otherwise it calculates the inverse for the new matrix and returns the inverse to makeCacheMatrix().

cacheSolve <- function(mat){
  inv<-mat$getinverse()
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  getmat<-mat$getmatrix()
  inv<-solve(getmat)
  mat$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}