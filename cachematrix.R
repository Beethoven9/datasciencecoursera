## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



# Using the provided example as a reference, this function will:
# 1- Create a special 'matrix' object
# 2- Cache the inverse of the matrix

makeCacheMatrix <- function(input_matrix = matrix()) {
  
  
  matrix_return <- NULL
  
  set <- function(matrix_store) #set the value of the matrix
  
    {
    input_matrix <<- matrix_store 
    matrix_return <<- NULL            
    }
  
  get <- function() input_matrix #get the value of the matrix
  setinverse <- function(solve) matrix_return <<- solve #set the value of the inverse
  getinverse <- function() matrix_return #get the value of the inverse
  
  list(set = set, get = get, #update the list
       setinverse = setinverse, 
       getinverse = getinverse)
  

}


## Write a short comment describing this function

# This function will
# Check to see if the inverse has already been calculated
#    If so, return the calculated  value and skip calculation
#    If not, calculate the inverse of the matrix and set the value
#            via the setinverse function

cacheSolve <- function(input_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'

  matrix_store <- input_matrix$getinverse()
  
  if(!is.null(matrix_store))
  {
    message("getting cached data") #print a message
    return(matrix_store) 
  }
  data <- input_matrix
  input_matrix$get()
  matrix_store <- solve(data, ...)
  input_matrix$setinverse(matrix_store)
  
  matrix_store
  
  }
