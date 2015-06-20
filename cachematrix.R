## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
       m <- NULL
      set <- function(y) {                            #set value of matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x
                                                      #get value of matrix
      
      setinverse <- function(inverse) m <<- inverse
                                                      #set value of inverse
      
      getinverse <- function() m
                                                      #get value of inverse
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
      
}


## Write a short comment describing this function
###This function computes the inverse of the special "matrix" returned by 
###makeCacheMatrix above. If the inverse has already been calculated 
###(and the matrix has not changed), then the cachesolve should retrieve the 
###inverse from the cache.

cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m                       ## Return a matrix that is the inverse of 'x'
      
}

#############     Below is what the test in my console looked like:

#     > 
#     > Test_Matrix <- matrix(c(-1, -2, 1, 1), 2,2)
#     > x <- makeCacheMatrix(Test_Matrix)
#     > x$get()
#          [,1] [,2]
#     [1,]   -1    1
#     [2,]   -2    1
#     > cacheSolve(x)
#          [,1] [,2]
#     [1,]    1   -1
#     [2,]    2   -1
#     > cacheSolve(x)
#     getting cached data
#          [,1] [,2]
#     [1,]    1   -1
#     [2,]    2   -1
#     > 