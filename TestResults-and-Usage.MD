# Test results and sample usage

## Load Script
Set working directory to directory containing script
or provide fully qualified reference to source file

	> setwd("/Users/hirenpatelatl/GitHub/ProgrammingAssignment2/")
	> dir()
	[1] "cachematrix.R" "README.md"    

	> source("cachematrix.R")

## Create Matrix to test with
	> matrix(c(1,2,3,4), nrow=2, ncol=2)
	     [,1] [,2]
	[1,]    1    3
	[2,]    2    4
	> a<-matrix(c(1,2,3,4), nrow=2, ncol=2)

## Solve matrix outside of function to determine expected output
	> solve(a)
	     [,1] [,2]
	[1,]   -2  1.5
	[2,]    1 -0.5

## Make cache matrix
	> a_cache <- makeCacheMatrix(a)

## Validate results in cache matrix match original matrix	
	> a_cache$get()
	     [,1] [,2]
	[1,]    1    3
	[2,]    2    4

## Solve matrix and cache result
	> cacheSolve(a_cache)
	     [,1] [,2]
	[1,]   -2  1.5
	[2,]    1 -0.5

## Pull result from cached value
	> cacheSolve(a_cache)
	getting cached data
	     [,1] [,2]
	[1,]   -2  1.5
	[2,]    1 -0.5