## makeCacheMatrix() wraps a matrix inside a closure that enables caching functionality.

## cacheSolve(matrix) computes from scratch (if called the first time)
## or retrieves from cache (if called subsequently) the value of the matrix inverse
## for the matrix wrapped with the makeCacheMatrix() function.

## This caching layer may be useful because the matrix inverse is a slow operation, so 
## it is advantageous to retrieve it from cache if it's already been computed
## - instead of repeating the inverse computation from scratch on each subsequent call.



## Function: makeCacheMatrix
## Description: constructs a closure (aka an outer function wrapper with its own scope)
##              for storing, setting, and accessing a cached value (e.g., matrix inverse)
## Arguments: matrix x (assumed to be invertible)
## Returns:
## R list containing the following methods:
##   set(y)      - initializes variable x in the closure to a new matrix y,
##                 resets cached inverse (xinv) in the closure to null
##   get()       - retrieves the saved matrix from the closure
##   setinv(inv) - sets the cached inverse (xinv) in the closure to inv
##   getinv()    - returns the cached inverse (xinv) stored in the closure

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL  ## stores the inverse of matrix x in the scope of the closure
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function: cacheSolve
## Description: computes (if called first time) or retrieves from cache (if called subsequently)
##              the value of the matrix inverse
## Arguments: 
##    x   - a closure created via makeCacheMatrix that wraps a matrix
##    ... - other (optional) arguments, which are passed directly to the R solve() method
## Returns: a matrix inverse for the matrix wrapped inside makeCacheMatrix()

cacheSolve <- function(x, ...) {
    # check if the inverse already exists in cache
    xinv <- x$getinv()
    # is yes, return the cached value
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    # otherwise, retrieve the original matrix that was wrapped inside makeCacheMatrix
    data <- x$get()
    # and compute its inverse using the solve() method
    xinv <- solve(data, ...)
    # save in cache
    x$setinv(xinv)
    # return
    xinv
}


###########################
#########  Tests   ########
###########################

# set a seed to be deterministic
set.seed(1)
# create a large square matrix with pseudo-random values
# CHOOSE A SMALLER VALUE if you are using a weak PC
n <- 1100;
m <- matrix(runif(n*n), n, n);

# create a closure around the matrix
mclos <- makeCacheMatrix(m)

# First call: data not cached, so this is SLOW
ptm <- proc.time()
inv1 <- cacheSolve(mclos)
print('First call:')
proc.time() - ptm

# Second call: data is already cached, so this is FAST
ptm <- proc.time()
inv2 <- cacheSolve(mclos)
print('Second call:')
proc.time() - ptm

# Third call: data is STILL cached, so this is FAST
ptm <- proc.time()
inv3 <- cacheSolve(mclos)
print('Third call:')
proc.time() - ptm

# lastly, as a sanity check, let's confirm that the inverse
# returned by our cached implementation of inverse is consistent
# with what we get by directly calling solve()
minv <- solve(m)
stopifnot(all(inv1 == minv))
stopifnot(all(inv2 == minv))
stopifnot(all(inv3 == minv))



#####################################
####### Additional tests  ###########
#####################################
# Here, we test that we can recycle the closure object
# to use it with a different matrix

# set a  DIFFERENT seed
set.seed(2)
# create a NEW large square matrix with pseudo-random values
n <- 1200;
m <- matrix(runif(n*n), n, n);

# RECYCLE existing closure by using its $set method
mclos$set(m)

# First call: data not cached, so this is SLOW
ptm <- proc.time()
inv1 <- cacheSolve(mclos)
print('First call:')
proc.time() - ptm

# Second call: data is already cached, so this is FAST
ptm <- proc.time()
inv2 <- cacheSolve(mclos)
print('Second call:')
proc.time() - ptm

# This call: data is still cached, so this is FAST
ptm <- proc.time()
inv3 <- cacheSolve(mclos)
print('Third call:')
proc.time() - ptm

# lastly, as a sanity check, let's confirm that the inverse
# returned by our cached implementation of inverse is consistent
# with what we get by directly calling solve()
minv <- solve(m)
stopifnot(all(inv1 == minv))
stopifnot(all(inv2 == minv))
stopifnot(all(inv3 == minv))



print('ALL DONE')