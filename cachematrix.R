######################################################################
## This program is run in R console and in three parts.
##
##      Part 1: Create a matrix
##      > xx <- rnorm(16)       # Generate 16 random normal variates
##      > dim(xx) <- c(4, 4)    # Use dim to make it a square matrix
##
##      Part 2: Create our special matrix
##      > x <- makeCacheMatrix(xx)      # This creates our special matrix
##
##      Part 3: Compute inverse of our matrix
##      > cacheSolve(x)
##
##      First time, when cacheSolve(x) is called, it uses
##      solve() to compute the inverse and caches using
##      x$setInvMatrix() method, that is implemented inside
##      makeCacheMatrix().
##
##      After this the subsequent runs of cacheSolve(x)
##      will fetch the cached matrix displaying a message
##      "Getting cached matrix"
######################################################################




###########################################################
##
##      Function:       makeCacheMatrix
##      Arguments:      A square matrix
##      Returns:        A list of methods or functions
##                      implemented inside this function
###########################################################
makeCacheMatrix <- function(x = matrix()) {

        invMat <- NULL

        getMatrix <- function() {
                return(x)
        }

        getInvMatrix <- function() {
                return(invMat)
        }

        setInvMatrix <- function(invertedMat) {
                return(invMat <<- invertedMat)
        }

        list(getMatrix = getMatrix,
             getInvMatrix = getInvMatrix,
             setInvMatrix = setInvMatrix)
}


###########################################################
##      Function:       cacheSolve
##      Arguments:      Arbitrary. Coerces to the type of
##                      passed argument
##      Returns:        An inverse matrix
##
###########################################################
cacheSolve <- function(x, ...) {

        dataMatrix <- NULL

        localInvMat <- x$getInvMatrix()

        if(!is.null(localInvMat)) {
                message("Getting cached matrix")
                return(localInvMat)
        }

        dataMatrix <- x$getMatrix()
        localInvMat <- (solve(dataMatrix) %*% dataMatrix)
        x$setInvMatrix(localInvMat)
        return(localInvMat)
}
