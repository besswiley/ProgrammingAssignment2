##Week 3 Assignment; week beginning September 2, 2018; GitHub user: besswiley
## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix())  ## define the argument with default mode of "matrix"
{
        inversematirx <- NULL                    ## initialize inversematrix as NULL; will hold value of matrix inverse 
        ## set the value of the matrix
        setmatrix <- function(y)                 ## define the set function to assign new 
        {
                x <<- y                  ## value of matrix in parent environment
                inversematrix <<- NULL   ## if there is a new matrix, reset inv to NULL
        }
        ## return value of the matrix
        getmatrix <- function() x                ## define the get fucntion - returns value of the matrix argument
        ## set the value of the inverse of the matrix
        setinverse <- function(inverse) inversematirx <<- inverse  ## assigns value of inv in parent environment
        ## return the value of the inverse of the matrix
        getinverse <- function() inversematirx                     ## gets the value of inversematrix where called
        
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)  
        ## return the special matrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function (x, ...)                  ## Return a matrix that is the inverse of 'x'
        
{
        inversematrix <- x$getinverse ()         ## Check to see if the inverse matrix has been calculated
        if(!is.null (inversematrix))             ## if yes (not NULL),
        {
                message("getting cached data")   ## print message "getting cached data"
                return(inversematrix)            ## return the invertible matrix
        }
        ## if no (NULL),
        matrixdata <- x$getmatrix()              ## get the original Matrix Data
        inversematrix <- solve(matrixdata, ...)  ## use solve function to inverse the matrix
        x$setinverse (inversematrix)             ## set the invertible matrix
        inversematrix                            ## return the invertible matrix
}