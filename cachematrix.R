#This function creates a specical "matrix" objectthat can cache it's inverse
#The first function, makeVector creates a special "vector", which is really 
#a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCachematrix <- function(x = matrix()) {
  matx <- NULL
  set <- function(y) {
    #Use  "<<-" to  assign a value to an object in an environment that is 
    #different from the current environment
    
    x <<- y
    matx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matx <<- inverse
  getinverse <- function() matx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following function calculates the mean of the special "vector" created with the above function

cacheinverse <- function(x, ...) {
  matx <- x$getinverse()
  
  #HOWEVER if the it first checks to see if the mean has already been calculated. If it has
  #it gets the mean from the cache, prints ou a line of text and skips the computation. 
  
  if(!is.null(matx)) {
    message("Getting cached data...")
    return(matx)
  }
  #Otherwise, it calculates the mean of the data and sets the value of the mean in 
  #the cache via the setmean function.  
  
  invdata <- x$get()
  matx <- solve(invdata,...)
  x$setinverse(matx)
  
  matx
}
