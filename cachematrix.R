## cacheMatrix.R contains:
##    R script code for Coursera Programming Assignment 2
##    comprised of makeCachematrix and cacheSolve functions plus
##    example test result from November 18, 2015 using RStudio
##  
## cacheMatrix.R computes the inverse of a matrix
## and caches it. It can re-use the cached inverse matrix rather than recompute the inverse 
## of the matrix. 
##
##
## cacheSolve computes the inverse of a matrix or obtains the inverse from cache
## by using the special matrix object created by makeCacheMatrix
##
## When a matrix is exactly singular, cacheMatrix fails and reports an error similar to the following message: 
##      Error in solve.default(mat.data, ...) : 
##      Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
##
## A successful test result (using example from http://www.wikihow.com/Find-the-Inverse-of-a-3x3-Matrix or use 3x3
## calculator at http://www.wolframalpha.com/widgets/view.jsp?id=35f68681262e42ea89b0834caa51635b)
##
##> m33 <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3)
##> mcm <- makeCacheMatrix(m33)
##
##> cacheSolve(mcm)
##     [,1] [,2] [,3]
##[1,]  -24   20   -5
##[2,]   18  -15    4
##[3,]    5   -4    1
##> cacheSolve(mcm)
##getting cached inverse
##     [,1] [,2] [,3]
##[1,]  -24   20   -5
##[2,]   18  -15    4
##[3,]    5   -4    1


## makeCacheMatrix is an R function that creates a special
## "matrix" object from a non singular matrix. It is an analogue of the makeVector function
## but for matrices rather than vectors. References in the makeVector function (and cachemean function)
## to the mean as mean or m have been replaced with m.inverse or m.inv respectively.
## Similarly functions set, get, setmean and getmean have been re-labeled for emphasis
## that the functions now are working with matrices.

makeCacheMatrix <- function(x = matrix()) {               ## argument x is a square, non singular matrix
 
   m.inv <- NULL                                          ## initialze matrix inverse
 
  setm <- function(y) {                                   ## mystery code carried over from makeVector
     x <<- y                                              ## but not used in it or here  
     m.inv <<- NULL                                     
  }
 
  getm <- function() x                                    ##obtain the matrix
 
  setm.inv <- function(m.inverse) m.inv <<- m.inverse     ##store the inverse matrix 

  getm.inv <- function() m.inv                            ##obtain the inverse matrix
 
  list(setm = setm, getm = getm,                          
       setm.inv = setm.inv,
       getm.inv = getm.inv)                               ##create the special matrix object
}


## cacheSolve is an R function that determines the inverse
## of the "special" matrix object determined by makeCacheMatrix
## or that retrieves the inverse from the cache when the inverse has already been
## calculated and the matrix has not changed. It is an analogue of
## the function cachemean, but for matrices rather than vectors.

cacheSolve <- function(x, ...) {                    ## argument x is the list obtained from makeCacheMatrix

    m.inv <- x$getm.inv()                                  ## assign inverse cache matrix or null to inverse matrix
 
  if (!is.null(m.inv)) {                            ## If cached inverse matrix exists
        message("getting cached inverse")
    return(m.inv)                                          ## return cached inverse 
   
  }                                                 ## Otherwise (meaning there is no cached inverse matrix for the current matrix)
 
  mat.data <- x$getm()                                     ## obtain the matrix requiring an inverse be computed
  
  m.inv <- solve(mat.data, ...)                            ## compute the inverse of a matrix using the solve function
  
  x$setm.inv(m.inv)                                        ## store the inverse in cache
  
  m.inv                                                    ## return just computed inverse 
  
}


