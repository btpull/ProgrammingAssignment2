## The folling two functions takes a matrix (makeCacheMatrix) as an input and returns its inverse (cacheSolve). 
## If the inverse has already been created, the cached object will be returned (makeCacheMatrix)
## otherwise a new object that is the inverse of the matrix will be created.
##
## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                   
             m <- NULL 
                  
             set <- function(y) {  # set the matrix 
  
                       x <<- y

                       m <<- NULL 
                     
                     }  
                
 		get <- function() x # gets the matrix
        
		setInvmatrix <- function(matrix) m <<- matrix #set m to be the inverse matrix, which is called in cacheSolve
        	
		getInvmatrix <- function() m
        
	   list(set = set, get = get,
             setInvmatrix = setInvmatrix,
             getInvmatrix = getInvmatrix)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

          m <- x$getInvmatrix() ## checks to see if the inveresed matrix exists

                if(!is.null(m)) {
                message("getting cached data")
                return(m)
        
		   }

        mt <- x$get() ## get the matrix

        m <- solve(mt) ## inverses the matrixs

        x$setInvmatrix(m) ## set the matrix

        m

}
