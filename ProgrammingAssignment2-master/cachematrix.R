## Put comments here that give an overall description of what your
## functions do

##below function will read matrix and create a struture
## with two field matrix and inverse of the matrix
## so we can save the matrix along with its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invr <- NULL
  
  # copy the given matrix in x and inverse of it is set to  NULL in invr variable
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  
  # Get the original matrix 
  get <- function(){
    x #same as return matrix x
  } 
  
  # Finf the matrix and store the result in invr variable
  setInverse <- function(inverse) {
    invr <<- inverse
  }
  
  # return the Inverse of the matrix if computed else return NULL
  getInverse <- function() {
    invr #return the return of the matrix    #same as return invr
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## compute the inverse of the matrix if it is not already computed and
## store it in the invr variable and if it is already computed then 
## read it from invr variable

cacheSolve <- function(x, ...) {
  invr <- x$getInverse()
  
  if(!is.null(invr)){ # if inverse is already computed
    return(invr)  # return the cached value
  }

  else{  # if not computed
    
    data <- x$get()
    inverse1 <- solve(data$x)
    setInverse(inverse1)
  }
  invr
  
}

