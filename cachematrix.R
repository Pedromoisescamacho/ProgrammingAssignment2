## this are two functions, the first one cache the
## input matrix. The second one solve the matrix (
## more details below)


## the first function just store the input matrix,
## in a structure that can be read by the second function,
## and also, in case that the function haven't been calcultated
##before, it store the inverse matrix when it is first 
##calculated in the second equation

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
         set <- function(y) {
                 x<<- y
                 inv <<- NULL
         }
         get <- function()x
         set_inv <- function(solve) inv <<- solve
         get_inv <- function() inv
         list(set = set, get = get,
              set_inv = set_inv,
              get_inv = get_inv)
 }

 ## first it test if martrix have been calculated before
## if it have been calculated just retrive this value 
## stored in the first equation. If it has not been calculated 
## calculates the inverse of the given matrix
 
 cacheSolve <- function(x, ...) {
         inv <- x$get_inv()
         if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
                 }
         data<- x$get()
         inv <- solve(data)
         x$set_inv(inv)
        inv
 }                           
         ## Return a matrix that is the inverse of 'x'