## These are two functions which cache the inverse of a matrix.

## This first one creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){

  ## Assign the NULL object to the variable inv
  inv <- NULL

  ## Set the matrix using another function.
  set <- function(y){
    x <<- y
    inv<<- NULL

    ## The double arrow operator makes that true in the parent as well, I think?
  }

  ## Get the matrix

  get <- function() {x}

  ## Set value of inverse.

  setInverse <- function(inverse) {inv <<- inverse}

  ## Get the value of the inverse once I have set it.

  getInverse <- function() {inv}

  ## List the methods

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Find inverse of the the special matrix returned by above function.
## If the inverse has already been calculated, it will just take this from the
##cache instead of doing the math again.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x' and assign it to inv

  inv <- x$getInverse()

  ##Check to see if the inverse has already been calculated.
  ## If so the function can get it from the cache and can avoid more work.

  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }

  ## Go get the matrix from our object
  mat <-x$get()

  ## To compute the inverse of a matrix, solve is the standard r function

  inv <- solve(mat) %% mat

  ##Now I set the value of the inverse in the cache using the setInverse function.

  x$setInverse(inv)

  ## Return the matrix
  inv
}
