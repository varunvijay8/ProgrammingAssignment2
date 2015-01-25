## makeCacheMatrix creates a matrix object and returns list
## of function:
## 1. Sets the matrix and initializes inverse 
## 2. Gets the matrix
## 3. Sets inverse
## 4. Gets inverse


makeCacheMatrix <- function(x = matrix()) {
  
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS....")
    install.packages("MASS")
    library("MASS")
  }
  
  invMx <- NULL
  set <- function(y) {
    x <<- y
    invMx <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invMx <<- inv
  getInv <- function() invMx
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates and sets the inverse matrix value using
## ginv funtion from MASS package
## Installs MASS package if not already installed

cacheSolve <- function(x, ...) {
  
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS.....")
    install.packages("MASS")
    library("MASS")
  }  
  
  invMx <- x$getInv()
  
  if(!is.null(invMx)) {
    message("getting cached data")
    return(invMx)
  }
  
  data <- x$get()
  invMx <- ginv(data)
  x$setInv(invMx)
  
  invMx
}