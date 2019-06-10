CacheMatrix <- function(a = matrix()) {
    c <- NULL
    set <- function(b) {            
        a <<- b                     
        c <<- NULL                  
    }
    get <- function() a
    setInvmatrix <- function(InvMatrix) c <<- InvMatrix
    getInvmatrix <- function() c
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(a, ...) {
        
    c <- a$getInvmatrix()              
    if(!is.null(c)) {           
        message("grabbing cache data")  
        return(c)                
    }
    data <- a$get()            
    c <- solve(data, ...)       
    a$setInvmatrix(c)            
    c                           
}
