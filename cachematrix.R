## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x<-NULL
  set <- function(y){
    x<<-y
    inv_x <<-NULL
  }
  get<-function()x
  setinv<-function(inv)inv_x<<-inv
  getinv<-function()inv_x
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x<-x$getinv()
  if(!is.null(inv_x)) {
    message("get cached inverse matrix")
    return(inv_x)
  }else{
    inv_x<-solve(x$get())
    x$setinv(inv_x)
    return(inv_x)
  }
}
