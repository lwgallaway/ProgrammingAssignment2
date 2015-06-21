## The purpose of these functions is to allow a user to save memory and time 
## by caching an inverted matrix into the global environment so that it can be
## recalled without having to re-compute that value.

## This function will set and retrieve a matrix and its inverse from the environment.
## The set portion is used to reassign a matrix to the environment if the 
## matrix you are using changes. The get allows the matrix that was inverted
## to be recalled. The setInv function will set a inversion of the matrix to
## the environment. The getInv function is used to retrieve an
## already inverted matrix from the environment.The functions are all set to a list
## so that it can be assigned to a object and that object can have all the functions.
##Note: no inverting is done inside this function.

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(h) {
        x <<- h
        inv <<- NULL
      }
      get <- function() x
      setInv <- function(solve) inv <<- solve
      getInv <- function() inv
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function is meant to take an invertable matrix and find its inverse and
## then store it to the global environment using the makeCacheMatrix functions. This function 
## will also check to see if that matrix inversion already exists, if so then it will
## return that value without making further calculations. Note: This function should be 
## used on a variable that makeCacheMatrix has already been assigned to. 
## EX. > a<-makeCacheMatrix(matrix(1:4,2,2))  > cacheSolve(a)

cacheSolve <- function( x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
        message("Retrieving the Matrix Inverse")
        return(inv)
      }
      info <- x$get()
      inv <- solve(info,...)
      x$setInv(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}
