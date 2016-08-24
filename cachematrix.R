## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
