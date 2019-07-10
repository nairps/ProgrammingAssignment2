
## CACHING THE INVERSE OF A MATRIX

# BACKGROUND: 
# Matrix inversion is a costly computation and caching the inverse of a matrix
# is beneficial if this computation needs to be performed repeatedly. 

# Two functions (details given below) were hence created with this objective
# to cache the inverse of a matrix.


#	1.	makeCacheMatrix: This function creates a special "matrix" object
#		(a list of four functions) which can cache the inverse of a square
#		matrix. The first function sets the value of the matrix, the second
#		gets the value of the matrix, the 3rd sets the value of the inverse
#		matrix and the last function gets the value of the matrix inverse.


#	2.	cacheSolve: This function computes the inverse of the special "matrix"
#		returned by makeCacheMatrix if not already calculated.
#		If the inverse has already been calculated for this matrix
#		(and the matrix has not changed), then cacheSolve retrieves
#		the inverse from the cache.


#
#	Further details and an example are provided in:
#		i) cachematrixDetails.md
#			and
#		ii) cachematrixDetails.html


## DEFINING THE FUNCTIONS:



makeCacheMatrix <- function(x = matrix()) {
##############################################################################
#
# Creates a special “matrix” object of class list which contains
# functions that can cache the inverse of “x”
#
# Args:
#   x: An invertible n-by-n square matrix 
#
# Returns:
# 
#	A list containing functions (N=4) that can cache the inverse of “x”. 
#	Specific role of each of these returned functions in this list are to: 	
#		1.	set the value of the matrix
#		2.	get the value of the matrix
#		3.	set the value of the matrix inverse
#		4.	get the value of the matrix inverse
# 
################################################################################

        #create a null object
        mat_inverse <- NULL
        
        #create the functions
        set <- function(y) {
                x <<- y
                mat_inverse <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) mat_inverse <<- solve
        getSolve  <- function() mat_inverse
   
        # return the above functions as a list
        list(
                set = set,
                get = get,
                setSolve = setSolve,
                getSolve = getSolve
        )
}

#making the makeCacheMatrix function call
mat <- makeCacheMatrix()

#check the class of the list elements
sapply(mat, class)






# cacheSolve:
# - This function returns the inverse of the matrix 'x'
#	provided to the makeCacheMatrix function.


cacheSolve <- function(x, ...) {
###################################################################################
# Computes the inverse of the special "matrix" object returned by makeCacheMatrix
#
# Args:
#   Special "matrix" object returned by makeCacheMatrix 
#
# Returns:
#   The inverse of the matrix provided to the makeCacheMatrix
#
####################################################################################

        # If the inverse is already calculated, retrieve it from the cache
        mat_inverse <- x$getSolve()
        if(!is.null(mat_inverse)) {
                message("getting cached data")
                return(mat_inverse)
        }
 
        # If not, get the matrix 
        mat <- x$get()
        
        # calculate its inverse  
        mat_inverse <- solve(mat, ...)
    
        # set the matrix inverse to the cache 
        x$setSolve(mat_inverse)
        mat_inverse
}

 
 
