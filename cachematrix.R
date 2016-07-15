## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(inp_matrix = matrix()) {

## initialize the inverse matrix         
    inverse_matrix <- NULL

## store input matrix in seperate environment    
        set <- function(input_matrix_new) {
        inp_matrix <<- input_matrix_new
        inverse_matrix <<- NULL
        }
## get the stored matrix from the seperate environment        
    get <- function() inp_matrix

## store the inverse of the matrix in seperate environment    
    setinvmatrix <- function(inv_matrix) inverse_matrix <<- inv_matrix

## get the inverse of the matrix from the seperate environment        
    getinvmatrix <- function() inverse_matrix

## print functions     
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)    
    
    

}


## This function computes the inverse of the "special matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve will retrieve the inverse from the cache.


cacheSolve <- function(special_matrix, ...) {
    

## check if inverse matrixed is cached an if yes, return its value    
    cached_invmatrix <- special_matrix$getinvmatrix()
    if(!is.null(cached_invmatrix)) {
        message("getting cached data")
        return(cached_invmatrix)
    }
## otherwise, the inverse matrix will be calculated, cached and returend    
    ## get input matrix from seperate environment    
    data <- special_matrix$get()
    ## calculate inverse matrix    
    cached_invmatrix <- solve(data, ...)
    ## cache inverse matrix
    special_matrix$setinvmatrix(cached_invmatrix)
    ## return inverse matrix
    cached_invmatrix
    
}
