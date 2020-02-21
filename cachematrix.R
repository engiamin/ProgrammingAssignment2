##################################################################################
## Put comments here that give an overall description of what your functions do ##
##################################################################################

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
# a matrix rather than compute it repeatedly.

# The below functions take advantage of lexical scoping to cache the inverse of a matrix and 
# utilize the fact that functions that return objects of type list() also allow access to any other 
# objects defined in the environment of the original function. 

# In the specific instance of makeCacheMatrix() this means that subsequent code can access the values of x
# or m (objects of the function) through the use of getters and setters. 
# This is how cacheSolve() is able to calculate and store the inverse for the input argument if it is 
# of type makeCacheMatrix(). 
# Because list elements in makeCacheMatrix() are defined with names, 
# you can access these functions with the $ form of the extract operator.


#####################################################
## Write a short comment describing this function  ##
#####################################################

# FIRST: makeCacheMatrix #
# This function creates a special "matrix" object that can cache its inverse.
# which is really a list containing a function to
        #1 set the value of the matrix
        #2 get the value of the matrix
        #3 set the value of the inverse
        #4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Step 1: Initialize objects
  m <- NULL
  
  ##Step 2: Define the "behaviors" or functions for objects of type makeCacheMatrix()
    #Step 2.1: Define setter to
              #a# Assign the input argument to the x object in the parent environment, and
              #b# Assign the value of NULL to the m object in the parent environment. 
                 set <- function(y) {
                                      x <<- y
                                      m <<- NULL
                                      }
    #Step 2.2: Define the getter for the matrix x
      get <- function() x
    #Step 2.3: Define the setter for the inverse 
      setinverse <- function() m <<- solve(x)
    #Step 2.4: Define the getter for the inverse
      getinverse <- function() m
  
  ##Step 3: Create a new object by returning a list()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#####################################################
## Write a short comment describing this function  ##
#####################################################

# SECOND: cacheSolve #
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve() should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ##Step 1: Call the getinverse() function on the input object.
  m <- x$getinverse()
  
  ##Step 2: Check to see whether the result is NULL. 
  if(!is.null(m)) {
    #if the value here is not equal to NULL, we have a valid, cached inverse and can return it to the 
    #parent environment
    message("getting cached data")
    return(m)
  }
  
  #If the result of !is.null(m) is FALSE cacheSolve() does the following...
  data <- x$get() #1# gets the matrix from the input object, 
  m <- solve(data, ...) #2# calculates an inverse,
  x$setinverse(m) #3# uses the setinverse() function on the input object to set the inverse in the input object,
  m #4# returns the value of the inverse to the parent environment by printing the inverse object.
}


######################
####### TRIAL ########
######################
funs <- makeCacheMatrix()
funs$set(matrix(1:4, 2))
funs$get()
funs$setinverse()
funs$getinverse()

cacheSolve(funs)