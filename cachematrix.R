# Carsten Langer, 2015-03-11
# Coursera Data Science Specialization, R Programming, Programming Assignment 2

## Some computations are resource intensive. If the result is needed several times,
## it is worth caching the result of the first computation and for the following calls
## just serve back the cached result.
## The 2 functions in this script provide such caching solution for the special case of
## calculating the inverse of a matrix.
## Basic concept taken from https://github.com/rdpeng/ProgrammingAssignment2 by Roger D. Peng

## Description
##   Provide object storage and getter/setter functions for a special "matrix" object to be used in cacheSolve().
## Usage
##   makeCacheMatrix(x = matrix())
## Aguments
##   x  matrix for which to create the special "matrix" object
## Value
##   List of 4 functions:
##     $set(new.matrix = matrix())          to set a new matrix into the already existing special "matrix" object
##     $get()                               to get the previously stored matrix
##     $set.inverse(new.inverse = matrix()) to set a new inverse matrix into the already existing special "matrix" object
##     $get.inverse()                       to get the previously stored inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ## initialize the internally stored inverse matrix
        inverse.matrix <- NULL

        ## set stores a new matrix to symbol x in makeCaseMatrix' environment, and resets the inverse matrix
        set <- function(new.matrix = matrix()) {
                x <<- new.matrix
                inverse.matrix <<- NULL
        }

        ## get the currently stored matrix
        get <- function() {
                x
        }

        ## set a new inverse matrix
        set.inverse <- function(new.inverse = matrix()) {
                inverse.matrix <<- new.inverse
        }

        ## get the currently stored inverse matrix
        get.inverse <- function() {
                inverse.matrix
        }

        ## return the list of the 4 functions
        list(set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)
}


## Description
##   Provide the inverse of a matrix via solve() and cache it.
## Usage
##   cacheSolve(x, ...)
## Aguments
##   x = special "matrix" object created by makeCacheMatrix()
##   ... = further options to solve()
## Value
##   The inverse of the original matrix stored in the special "matrix" object
## Details
##   It is assumed that the original matrix is invertible.
##   If the inverse was calculated before, and the original matrix was not changed,
##   the result is taken from the cache and a message is raised.
cacheSolve <- function(x, ...) {
        ## Check if inverse matrix is already stored
        inverse <- x$get.inverse()
        if(!is.null(inverse)) {
                ## Yes, so return the cached matrix.
                message("Inverse taken from cached data.")
                return(inverse)
        }
        ## No, so calculate the inverse, cache it and return it.
        matrix <- x$get()
        inverse <- solve(matrix, ...)   ## Resuse the ... arguments
        x$set.inverse(inverse)
        inverse
}

## Unit and function tests, just run tests(); will stop with error if a test fails.
tests <- function() {
        stopifnot(identical(matrix(), makeCacheMatrix()$get()))
        stopifnot(identical(NULL, makeCacheMatrix()$get.inverse()))
        m <- matrix(1:4, 2, 2)
        cm <-makeCacheMatrix(m)
        stopifnot(identical(m, cm$get()))
        stopifnot(identical(NULL, cm$get.inverse()))
        i <- solve(m)
        stopifnot(identical(i, cacheSolve(cm)))
        stopifnot(identical(i, cm$get.inverse()))
        stopifnot(identical(i, cacheSolve(cm)))  ## difficult to check if this time cache was used.
        m <- matrix(4:1, 2, 2)
        cm$set(m)
        stopifnot(identical(m, cm$get()))
        stopifnot(identical(NULL, cm$get.inverse()))
        i <- solve(m)
        stopifnot(identical(i, cacheSolve(cm)))
        stopifnot(identical(i, cm$get.inverse()))
        message("If text \"Inverse taken from cached data.\" appears once above, then all tests succeeded!")
}
