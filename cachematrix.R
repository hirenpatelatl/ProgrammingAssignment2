## The following two functions illustrate an example of creating an object
## that can store both an original input value and a solved computational result
## This is done so computational intensive tasks are not unecessarly repeated.

## MakeCacheMatrix: Is a vehicle for populating an object with a matrix
## and storing the results of solved value
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: Determines is the object that has been passed to it has a solved value
## If it has a solved value it will return the solved value and not perform any further computation.
## If not it will solve the inverse of the matrix store in the object. Set the solved
## value in its appropriate member and then return the solved value.

## 1.    Takes in makeCacheMatrix object
## 2.    Determines if the object has a solved value
## 2.a   If it has a value it will display/return the value and exit 
## 2.b.1 If it does not have a solved value it determines the
##       inverse of the matrix
## 2.b.2 Then it stores it back in the object that was passed to it
## 2.b.3 Finally, it returns the solved value and exits

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m        
}

# Test results and sample usage
## Load Script
# Set working directory to directory containing script
# or provide fully qualified reference to source file
#
#	> setwd("/Users/hirenpatelatl/GitHub/ProgrammingAssignment2/")
#	> dir()
#	[1] "cachematrix.R" "README.md"    
#
#	> source("cachematrix.R")
#
## Create Matrix to test with
#	> matrix(c(1,2,3,4), nrow=2, ncol=2)
#	     [,1] [,2]
#	[1,]    1    3
#	[2,]    2    4
#	> a<-matrix(c(1,2,3,4), nrow=2, ncol=2)
#
## Solve matrix outside of function to determine expected output
#	> solve(a)
#	     [,1] [,2]
#	[1,]   -2  1.5
#	[2,]    1 -0.5
#
## Make cache matrix
#	> a_cache <- makeCacheMatrix(a)
#
## Validate results in cache matrix match original matrix	
#	> a_cache$get()
#	     [,1] [,2]
#	[1,]    1    3
#	[2,]    2    4
#
## Solve matrix and cache result
#	> cacheSolve(a_cache)
#	     [,1] [,2]
#	[1,]   -2  1.5
#	[2,]    1 -0.5
#
## Pull result from cached value
#	> cacheSolve(a_cache)
#	getting cached data
#	     [,1] [,2]
#	[1,]   -2  1.5
#	[2,]    1 -0.5



