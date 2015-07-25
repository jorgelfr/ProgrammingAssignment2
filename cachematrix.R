## These functions calculates the inverse of a matrix. First functions receives and cache the inverse matrix. 
## Second function verifies if inverse matrix has been calculated, if not inverse matrix is calculated.

## This function set the matrix mat and then calculates the inverse of matrix mat.
## Then set the inverse matrix and get the value of the inverse matrix.


## function makeCacheMatrix is created an receive as argument a matrix.
makeCacheMatrix <- function(Mat = matrix())
{
     Matinv <- NULL  ## MatInv is set as a Null element
     set <- function(y) ## Set function is created with y as an argument
     {
          Mat <<- y  ## Mat is assigned in the parent environment
          Matinv <<- NULL ## Mat is assigned as null in the parent environment
     }
     get <- function() Mat ## get is assigned with invisible function Mat
     setInv <- function(inv) Matinv <<- inv ## setInv is assigned with invisible function with argument inv and MatInv is set inv value in the parent environment
     getInv <- function() Matinv ## getInv is assigned with invisible function MatInv
     list(set = set, get = get, setInv = setInv, getInv = getInv) ## A list is created with elements, set, get, setInv, getInv with default values
}


## This function gets the inverse matrix. If matrix has been already calculated, it retrieves the matrix from the parent
## environment.

cacheSolve <- function(Mat, ...) 
{
     m <- Mat$getInv() # From the list Mat getInv is readed
     if(!is.null(m)) # It is checked if inverse matrix has been calculated
     {
          message("getting cached data") # if inverse matrix has been calculated it retrieves the information that is allocated on memory
          return(m) # ends function
     }
     data <- Mat$get() # If matrix has not been calculated, matrix is send for calculation
     m <- solve(data) # It calculates inverse matrix
     Mat$setInv(m)  # It saves the inverse matrix
     print(m)
}
