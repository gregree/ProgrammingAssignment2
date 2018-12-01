## This is to solve the programming assignment for week 3rd 
## of the Coursera course 'Programming in R' - Chaching inverted matrix for further calculations
## It can be checked by the procedure proposed by Alan Berger under link
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg, 
## but names of the functions must be updated as in the assignment I had renamed objects and functions
## in order to better reflect the assignment's context

## This function creates a special "matrix" object that can cache its inverse. 'x' is the matrix to be inverted. 'inversion' 
##is an object where the inverted matrix is to be kept. There are also declared 4 sub-functions within the function
## which will allow operations with the 'inversion' object:
## - 'set.matrix' is to define the value of new matrix to be inverted (other than the one defined by the first use of makeCachematrix
## function), restatement of 'm' back to value of 0 cleans the inverse of matrix cached earlier - it is due to change
## of the object subject to inversion that occurs due to each calling of makeCacheMatrix function
## - 'get.matrix' - returns the matrix to be subject of inversion
## - 'set.inverted' - inverts matrix and caches it
## - 'get.inverted' - returns the inverted matrix
## Last element of the function is creating a list that collects the sub-functions of the function makeCacheMatrix and makes 
## them available for use in parent environment

makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        set.matrix <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        get.matrix <- function() x
        set.inverted <- function(solve) inversion <<- solve
        get.inverted <- function() inversion
        list(set.matrix = set.matrix, get.matrix = get.matrix,
             set.inverted = set.inverted,
             get.inverted = get.inverted)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## In the first section the function checks whether the 'x' has been already inverted. If it has 
## (i.e. !is.null(inversion)) realizes there existx an inverted matrix for 'x'
## then cached inverted matrix is returned.
## in the second section of the function a new object 'data' is created and new matrix 'x' is defined. 'x' is then inverted 
## (by using solve function) and after it new cached matrix is set up (by set.inverted). In the end inversion object is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversion <- x$get.inverted()
        if(!is.null(inversion)) {
                message("getting cached data")
                return(inversion)
        }
        data <- x$get.matrix()
        inversion <- solve(data, ...)
        x$set.inverted(inversion)
        inversion
}
