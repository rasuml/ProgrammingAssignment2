
## Function makeCacheMatrix can make a special matrix which contain the values of matrix and inverse of that matrix. Typical usage of the functionis as following example:
## 1. create a matrix: examplemat <- matrix(1:4, nrow = 2, ncol = 2)
## 2. Make a dummy special matrix: examplematMatrix <- makeCacheMatrix()
## 3. Initialized the special matrix: examplematMatrix$set(examplemat)

makeCacheMatrix <- function(x = matrix()) {
  #Set the cached inverse varible "inv" to NULL
  inv <- NULL 
  # This function initilizes the "Special" Matrix
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  # This function returns the matrix itself
  get <- function() x
  # This function stores the calculated inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  # This function retrieves the cached inverse of the matrix
  getinv <- function() inv
  # Return this "special" matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  

}


## This function retrive the inverse of the special matrix x depending on if there is value stored in the cached varible 

cacheSolve <- function(x, ...) {
  
  # Get the stored value from the special matrix.
  inv <- x$getinv()
  # The value is not NULL and thus, indicating that the value have already calculated. 
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # The value is NULL, calculate it and store it in the special matrix
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
