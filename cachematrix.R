## This is a Coursera R programming assignment where I will create
## a function that caches the inverse of matrices

## The funtion will first create a function that generates a list
## of functions that will set and get the inverse of the matrix in arg.

makeCacheMatrix <- function(x = matrix()) { #grab the input matrix
      invs <- NULL # create an empty vector for later cacheing
      set <- function(y) { #the set matrix value function
            x <<- y # pull the y var. into the parent envirion.
            invs <- NULL # clr for clearing cache from previous input
            # if any
      }
      get <- function() x # get function; retrieve input-x from parent
      setinv <- function(solve) invs <<- solve # pull inverse result
      # from cachematrix - up next - into the parent environ 
      getinv <- function() invs  # FUN to retrieve invs from setinv
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv) # name the functions accordingly
}


## the 2nd part of the cache function is to actually calculate within
## the setup of makeCacheMatrix

cacheSolve <- function(x, ...) { # pass x from FUN above as args 
      ## Return a matrix that is the inverse of matrix 'x'
      invs <- x$getinv() # check if the matrix var is already in cache
      if(!is.null(invs)) { # not NULL == existent, it's there! 
            message("found it!")
            return(invs) # pull the solution from cache
      }
      # when invs is NULL, or solution not there
      p <- x$get() # get/subset the matrix input from makeCacheMatrix
      # and initiate into cachematrix FUN
      invs <- solve(p, ...) %*% p # solve the inverse
      x$setinv(invs) # place the solution into cache
      invs #print inversed matrix
}


# testing
set.seed(1)
d <- matrix(rnorm(1:25, 25), 5, 5)
sd <- solve(d) %*% d #inverse of matrix d

dmat <- makeCacheMatrix(d) # same matrix but now cache-able
cacheSolve(dmat)
cacheSolve(dmat) #2nd call to prove it's cached

invsd <- cacheSolve(dmat)
all.equal(sd, invsd) # test to see if the solutions are the same


## special thanks and acknowledgement to Igreski on Github for
## explaining the assignment step-by-step
## (datasciencectacontent/markdown/rprog-breakingDownMakeVector.md)
## and EndMemo for using 'solve' to get the inverse of a matrix
## (http://www.endmemo.com/program/R/solve.php)
