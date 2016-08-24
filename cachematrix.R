## Put comments here that give an overall description of what your
## functions do
##  #This program caches the inverse matrix.

## Write a short comment describing this function
##  #The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##      - set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Initiate check: Checking matrix is square matrix or not. If it is not a square matrix, the program will stop. 
        chk.matrix <- dim(x)
        if(chk.matrix[1] != chk.matrix[2]){
                stop("This is not a square matix. Input must be a square matrix. This message is approved by Dhairya Mistry :)")
        }
        return()
        #stop square matrix check. Above check section is not part of assignment. It is something extra I did for fun!
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinverse = setinv,
             getinverse = getinv)
}


## Write a short comment describing this function
##      The following function calculates the inverse of the special "matrix" created with the above function. 
##      However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
##      the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
##      of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
