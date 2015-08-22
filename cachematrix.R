## Put comments here that give an overall description of what your
## functions do

## This function is contains functions for working with matrices

makeCacheMatrix <- function(x = matrix()) {
    ## m is a 1 x 1matrix with NA in each spot by the default setting of matrix()
    m <- matrix()
    ## set will take a matrix and set x to equal it. m is set to matrix with NA in each element to state the inverse has not been taken
    set <- setfunction(y) {
       x <<- y
       m <<- matrix(data=NA)
    }
    ## get simply gets a matrix x
    get <- function() x
    ## setinverse will set m to be a matrix,it does not solve x to make m
    setinverse <- function(solve) m <<- solve
    ## get inverse will return m
    getinverse < - function() m
    list( set=set,get=get,setinverse=setinverse, getinverse=getinverse)
} 


## This function will return the inverse matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## m[1,1] is only NA if the inverse has not been taken yet,  therefore this returns the cached inverse if it m[1,1] is not na
    if(!is.na(m[1,1])){
      ## displays a message saying it was retrieved from cache
      message("returning cache matrix")
      ## returns the previously calculated inverse matrix
      return m
    }
    ## uses get function from make cache function to set data to matrix x
    data <- x$get()
    ## uses solve function on matrix data and set m to the solution
    m <- solve(data,...)
    ## uses set inverse funcetion from make cache to store the inverse of x
    x$setinverse(m)
    ## returns the inverse matrix
    m
}
