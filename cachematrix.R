
# makeCacheMatrix takes a Matrix and creates its Inverse Matrix using Solve()
# it also defines 4 functions to let the user get,set Matrix and get,set a matrix's
# inverse


makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(solve) invMatrix<<- solve
    getInv <- function() invMatrix
    
    list(set=set,get=get,setInv=setInv,getInv = getInv)
}


## cacheSolve is a function that caches the inverse of a matrix
## it starts by testing whether the inverse matrix is calculated so that it caches
## it, otherwise it calculates the inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data,...)
    x$setInv(invMatrix)
    invMatrix
}
