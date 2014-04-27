###############################################################################################
##JORGE ALBERTO VILLALTA MONTENEGRO 
###############################################################################################
## Those functions calculates the inverse matrix and store it in the cache
## using 2 special objects;
#      "x" special object that stores the matrix in the cache
#      "i" special object that stores the inverse matrix in the cache
# Created two function:
##########################################################################################
# 1.  makeCacheMatrix: with this function create the special objetcts (i, x),
#                       make "i" null and create the list of functions
##########################################################################################
makeCacheMatrix <- function(x = matrix()) {# Uses the "x" argument that will recieve the matrix that it'll use
  i<-NULL                                  # makes NULL the "i" special object
  set<-function(y=matrix()){               # Defines the first function that it'll use if you want to set a new matrix
    x<<-y                                  # with the "y" argument it recieves the new matrix and assigns to the special object "x" in the cache
    i<<-NULL                               # makes the other special object "i" NULL
    print(x)                               # prints the new value of "x" special object
  }                                        # end first function
  get <- function() x                      # Second function, it shows the value of "x" special object
  setsolve <- function(inv) i<<-inv        # Third function, it assigns to "i" special object the value of "inv" argument
  getsolve <- function () i                # Fourth function, it shows the value of "i" special object
  list(set=set,get=get,                    # It creates the list of my functions
      setsolve=setsolve,
      getsolve=getsolve)
}

###############################################################################################
## 2.  cacheSolve: computes the inverse of the special "matrix" (x) OR retrieve the inverse from the cache.
##########################################################################################

cacheSolve <- function(x, ...) {        # Uses the argument "list" for recieve the list of function below
  inv<-x$getsolve()                     # Assigns to "inv" the value cached of "i" special object (execute getsolve function)
  if(!is.null(inv)){                    # Verificates if the value it's NULL
    message ("getting cached data")     # If it's TRUE, shows the message
    return(inv)                         # Return a matrix that is the inverse of 'x'
  }                                     # End If... If its FALSE its because "i" doesn't have a value and it has to computes the inverse
  data<-x$get()                         # For that, Assigns to "data" the value cached of "x" special object (execute get function)
  inverse<-solve(data)                  # Compute the inverse matrix (solve) and It store to "inverse" variable
  x$setsolve(inverse)                   # with the new value of the inverse matrix, executes the "setsolve" function with "inv" argument for assignt to "i" in the cache
  inverse                               # Return a matrix that is the inverse of 'x',that it the same of "i" special object 
}
