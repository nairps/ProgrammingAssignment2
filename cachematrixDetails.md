Background
----------

Matrix inversion is a costly computation and caching the matrix inverse
is beneficial if this needs to be calculated repeatedly.

To this end, two functions were created to cache the inverse of a given
matrix. A third function was created to compare the time taken for the
inverse matrix calculation with that taken for retrieving the inverse
matrix from the cache.

Assumptions
-----------

Matrix supplied is always square and invertible.

Functions
---------

### 1. `makeCacheMatrix`

This function creates a special "matrix" object of class `list`, which
comprises of functions (N=4), that can cache its inverse. Specifically,
the first function sets the value of the matrix and the second function
gets the value of the matrix. The 3rd function sets the value of the
inverse matrix and the last one gets the value of the matrix inverse.

    makeCacheMatrix <- function(x = matrix()) {
    ##############################################################
    # creates a special "matrix" object that can cache its inverse
    #
    # Args:
    #   x: An invertible n-by-n square matrix   
    #
    # Returns:
    # 
    # A list containing functions to:
    #   1. set the value of the matrix
    #   2. get the value of the matrix
    #   3. set the value of the matrix inverse
    #   4. get the value of the matrix inverse
    # 
    ##############################################################

            #create a null object
            mat_inverse <- NULL
            set <- function(y) {
                    x <<- y
                    mat_inverse <<- NULL
            }
            get <- function() x
            setSolve <- function(solve) mat_inverse <<- solve
            getSolve  <- function() mat_inverse
       
            # return the created functions as a list
            list(
                    set = set, 
                    get = get,
                    setSolve = setSolve,
                    getSolve = getSolve
            )
    }

    # a makeCacheMatrix function call
    mat <- makeCacheMatrix()

    #get the class of the list elements
    sapply(mat, class)

    ##        set        get   setSolve   getSolve 
    ## "function" "function" "function" "function"

### 2. `cacheSolve`

This function retrieves the inverse of the matrix 'x' provided to
`makeCacheMatrix` from the cache and returns it, if this has already
been calculated (and the matrix has not changed). Else, it computes the
inverse of the matrix 'x' provided to `makeCacheMatrix`, sets the
inverse to the cache and returns the matrix inverse.

    cacheSolve <- function(x, ...) {
    ###################################################################################
    # Computes the inverse of the special "matrix"" object returned by makeCacheMatrix() 
    #
    # Args:
    #   Special "matrix"" object returned by makeCacheMatrix()  
    #
    # Returns:
    #   The inverse matrix of 'x'
    #
    ####################################################################################
          
            # If the inverse is already calculated, retrieve it from the cache and return
            mat_inverse <- x$getSolve()
          
            if(!is.null(mat_inverse)) {
                    message("getting cached data")
                    return(mat_inverse)
            }
     
            # If not, get the matrix      
            mat <- x$get()
            
            # calculate the inverse        
            mat_inverse <- solve(mat, ...)
        
            # set the matrix inverse to the cache       
            x$setSolve(mat_inverse)
            
            # return the inverse matrix
            mat_inverse
    }

### 3. `testInvTime`

`testInvTime` compares the time taken for matrix inverse calculation
with the time needed for retrieving matrix inverse from the cache using
the above created functions. The time comparisons are performed by this
function for one invertible n-by-n random square matrix at a time, where
'n' is given as input.

    testInvTime <- function (n = 5){
    ##################################################################################
    #   
    # Args:
    #   The dimension (n) for generating a random square matrix
    #
    # Returns:
    #   Data frame with 
    #       (1) The dimension (n) of the random square matrix
    #       (2) The time taken for the matrix inverse calculation
    #       (3) The time taken for the retrieval of inverse matrix
    #      
    ################################################################################### 

            # create the n-by-n square matrix
            set.seed(2)
            mat <- matrix(
            rnorm(n*n), 
            n, n
            )
        
            #check if the matrix is invertible
            deter <- det(mat)
            if (deter == 0) {
                    stop("matrix not invertible")
            }

            #total time for computing the matrix inverse
            time0 <- Sys.time()
            matFunc <- makeCacheMatrix(mat)
            inverse <- cacheSolve(matFunc)
            inv_time <- Sys.time() - time0
        
            #time taken for matrix inverse retrieval from the cache
            time1 <- Sys.time()
            inverse2 <- cacheSolve (matFunc)
            cache_time <- Sys.time() - time1
          
            #return the results as a data frame
            data.frame(
                    "matrix_dim" = n,
                    "inverse_time" = inv_time,
                    "cache_time" = cache_time
                    )
    }

### Time comparisons

    #######################################################################
    #
    #   - Matrix inverse calculation time versus cache retrieval time
    #
    #   - designed for n-by-n random square matrices
    #
    #   - where n = seq(1000,2500, 100)
    #
    #######################################################################
    times <- NULL
    options(scipen = 999)
    for (i in seq(1000,2500, 100)){ 
            print(i); 
            time <- testInvTime(i); 
            times <- rbind(times, time)
    }

    knitr::kable(
            times, 
            align = c('l'),
            caption = "Matrix inverse calculation time versus Cache retrieval time",
            col.names = c(
                    "Square matrix dimension (n)", 
                    "Time for matrix inverse calculation", 
                    "Cache retrieval time")
    )

<table>
<caption>Matrix inverse calculation time versus Cache retrieval time</caption>
<thead>
<tr class="header">
<th align="left">Square matrix dimension (n)</th>
<th align="left">Time for matrix inverse calculation</th>
<th align="left">Cache retrieval time</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1000</td>
<td align="left">0.9326398 secs</td>
<td align="left">0.0350552 secs</td>
</tr>
<tr class="even">
<td align="left">1100</td>
<td align="left">1.3188629 secs</td>
<td align="left">0.0002348 secs</td>
</tr>
<tr class="odd">
<td align="left">1200</td>
<td align="left">1.6626542 secs</td>
<td align="left">0.0002379 secs</td>
</tr>
<tr class="even">
<td align="left">1300</td>
<td align="left">2.3143311 secs</td>
<td align="left">0.0003819 secs</td>
</tr>
<tr class="odd">
<td align="left">1400</td>
<td align="left">2.7808781 secs</td>
<td align="left">0.0004299 secs</td>
</tr>
<tr class="even">
<td align="left">1500</td>
<td align="left">3.3342290 secs</td>
<td align="left">0.0002458 secs</td>
</tr>
<tr class="odd">
<td align="left">1600</td>
<td align="left">3.9811690 secs</td>
<td align="left">0.0004911 secs</td>
</tr>
<tr class="even">
<td align="left">1700</td>
<td align="left">4.8320150 secs</td>
<td align="left">0.0002789 secs</td>
</tr>
<tr class="odd">
<td align="left">1800</td>
<td align="left">5.8496850 secs</td>
<td align="left">0.0003688 secs</td>
</tr>
<tr class="even">
<td align="left">1900</td>
<td align="left">6.9245090 secs</td>
<td align="left">0.0002360 secs</td>
</tr>
<tr class="odd">
<td align="left">2000</td>
<td align="left">7.7993598 secs</td>
<td align="left">0.0002789 secs</td>
</tr>
<tr class="even">
<td align="left">2100</td>
<td align="left">9.1604729 secs</td>
<td align="left">0.0002501 secs</td>
</tr>
<tr class="odd">
<td align="left">2200</td>
<td align="left">10.5417280 secs</td>
<td align="left">0.0002358 secs</td>
</tr>
<tr class="even">
<td align="left">2300</td>
<td align="left">11.8521030 secs</td>
<td align="left">0.0002558 secs</td>
</tr>
<tr class="odd">
<td align="left">2400</td>
<td align="left">13.1426370 secs</td>
<td align="left">0.0002329 secs</td>
</tr>
<tr class="even">
<td align="left">2500</td>
<td align="left">14.9107182 secs</td>
<td align="left">0.0002592 secs</td>
</tr>
</tbody>
</table>
