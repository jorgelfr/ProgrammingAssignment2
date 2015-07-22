## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(Mat = matrix())
{
     Matinv <- NULL
     set <- function(y)
     {
          Mat <<- y
          Matinv <<- NULL
     }
     get <- function() Mat
     setInv <- function(inv) Matinv <<- inv 
     getInv <- function() Matinv
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(Mat, ...) 
{
     m <- Mat$getInv()
     if(!is.null(m))
     {
          message("getting cached data")
          return(m)
     }
     data <- Mat$get()
     m <- solve(data)
     Mat$setInv(m)
     print(m)
}
