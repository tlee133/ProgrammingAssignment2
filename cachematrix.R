## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates a object which can be used to store a matrix and its 
# inverse.  Calling the cacheSolve function on the makeCacheMatrix object will
# solve for the inverse matrix of the matrix in the makeCacheMatrix object and 
# save it to that object.  The cacheSolve function will also return the inverse of
# the matrix. If the cacheSolve function is called more than once it will return
# the cached (previously calculated) inverse matrix.

# Function: makeCacheMatrix
# Input: Invertable Matrix
# Output: Matrix Object to cache a matrix and its inverse.
# Notes: cacheSolve must be called on this object at least once to set the 
# Inverse matrix. 
makeCacheMatrix <- function(x = matrix()) {
  # Initialize variables.  Dim of inverse matrix will be 1x1
  # upon creation of matrix object.
  inverse <- matrix()
  
  # Set Value of matrix object.
  set <- function(y){
    x <<- y
    inverse <<-matrix()
  }
  
  # Function: Return Original Matrix
  get <- function() x
  
  # Function: Set inverse to inv.
  setinv <- function(inv) inverse <<- inv
  
  # Function: Return Inverse Matrix
  getinv <- function() inverse
  
  # Return Values
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function: cacheSolve
# Input: makeCacheMatrix Object
# Output: Returns the inverse of the matrix stored in makeCacheMatrix object.
# Notes: Updates the inverse matrix of the input object if not available.  The
# Function will print to console when retrieving cached data.
cacheSolve <- function(x = makeCacheMatrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get current value of inverse matrix.
  inv <- x$getinv()
  
  # If inverse and original are equal, the inverse matrix has been populated.
  if (sum(dim(x$get())==dim(x$getinv())) == 2)  {
    # Check if matrix is equal to default inverse dimensions (1x1).
    # There's a chance the value of the inverse is NA.
    if (sum(dim(x$get()) == c(1,1)) == 2){   
      # Check if 1x1 inverse matrix is NA - if yes, create the inverse matrix.
      if(is.na(x$getinv())){
        mat = x$get()
        inv = solve(mat)
        x$setinv(inv)
        inv
      }
      # Otherwise return cached 1x1 inverse.
      else{
        message("getting cached data")
        return(inv)
      }
    }
    # Otherwise return the inverse matrix. 
    else{
      message("getting cached data")
      return(inv)
    }
  }
  # If dimensions do not match, calculate the inverse matrix, set the value
  # of the inverse matrix in the makeCacheMatrix Object, and
  # return the inverse matrix.
  else{
    mat = x$get()
    inv = solve(mat)
    x$setinv(inv)
    inv
  }
}
