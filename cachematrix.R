#you can call the two functions as described below, were mat is the matrix who's inverse you
#want to calculate and cache

##cacheSolve(makeCacheMatrix(mat))

#The first function, makeCacheMatrix creates a special "matrix", which is really 
#a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL#variable for the inverse matrix
  #set for the input matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x #simple getter for the input matrix
  setInv <- function(inv) i <<- inv #assigns the value of inv to i in an environment that is may be
  #different from the current environment
  getInv <- function() i#gets the value of the input matrix
  #returns a list of all enviroment variables and functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#The following function calculates the inverse matrix of the special "matrix" created with 
#the above function. However, it first checks to see if the inverse has already been
#calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
#the cache via the setInv function.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {#if i is null, we must compute the inverse
    #otherwise simple return it's value
    message("getting cached data")
    return(i)
  }
  data <- x$get()#get the input matrix
  i <- solve(data, ...)#compute inverse matrix
  x$setInv(i)
  i
}