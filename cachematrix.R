## R Script to get the Inverse of the matrix using Solve function 
## Author: Linesh Badole
## Version: 1.0 # Date : 3/24/2015
## Aim : write a pair of below functions that cache the inverse of a matrix
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#			If the inverse has already been calculated (and the matrix has not changed), 
#			then the cacheSolve should retrieve the inverse from the cache.


# makeCacheMatrix: Function 
makeCacheMatrix <- function(x = matrix()) # x: Argument to receive the matrix 
{  
        # i: Argument to keep the inverse of the matrix 
        i <- NULL  # set it to NULL for the first time 
        
        set <- function(y) { # function set: to set the value of matrix if called using arg$set(y = matrix)
                x <<- y    # set the value of matrix to x
                i <<- NULL # set the null value to inverse i 
        }
        
        get <- function() x   # returns the matrix exa. arg$get() 
        
        setInverse <- function(solve) i <<- solve   # set the value of the solve function
        
        getInverse <- function() i  # get the value of the solve function
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # create a list of all the functions set,get,setInverse,getInverse
}

# cacheSolve : function

cacheSolve <- function(x, ...)  # Input the special list created from makeCacheMatrix function
{
        i <- x$getInverse()  # call the getInverse function to check if the result was calculated before
        if(!is.null(i)) { # check if result from above x$getInverse function in i is not NULL means result was cached
                message("getting cached data") 
                return(i) # return cached inverse result and stop the function
        }
        # else data wasn't cached run the inverse and cache the result
        data <- x$get() # get the new matrix using x$get()
        i <- solve(data, ...) # calculate the inverse of the matrix 
        x$setInverse(i) # save the inverse of the matrix in 
        i # print the inverse of the matrix 
}

##Example.

# 1. set the 2 * 2 matrix (only square matrix) can be calculated through solve function

# > m <- matrix(c(1,2,3,4),nrow=2,ncol=2); # matix m 

# 2. call the makeCacheMatrix function using m as an argument and save the returned list of functions in argument "a" 

# > a <- makeCacheMatrix(m);
# > a$get(); # prints out the matrix m
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

# 3. Call the function cacheSolve using "a" as an argument

# > cacheSolve(a); # you get the inverse of the matrix m 
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# 4. run the cacheSolve(a) to check if we get cached result 

# > cacheSolve(a)
# getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

