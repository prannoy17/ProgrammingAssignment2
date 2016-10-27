##This function is used to create the "special matrix".
##It is a list containing the following functions :
## 1. Setting the value of the vector
## 2. Getting the value of the vector 
## 3. Setting the value of the inverse
## 4. Getting the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The follwoing function is used to get the inverse of a matrix.
## It first checks to see if the inverse of a matrix is already calculated or not. If so, then it skips the computation
## and get the inverse of a matrix from the cached data. Otherwise it will calculate
## the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data of the inverse of a matrix")
    return(m)
  }
  data<-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
