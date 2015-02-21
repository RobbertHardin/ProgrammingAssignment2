## This file contains two functions: makeCacheMatrix() and 
## cacheSolve(). makeCacheMatrix() can be used to store the 
## inverse of a matrix. cacheSolve() return the inverse of
## a makeCacheMatrix matrix by either retreiving it from
## its cache, or, if it's not in its cache, by using solve().


## makeCacheMatrix() accepts a matrix as argument. Next
## you can use getInverse() and setInverse() to getInverse
## get and set its inverse. You can also use get and set
## to directly set the matrix. Note that if you set a 
## matrix this way, you also have to set the inverse !

makeCacheMatrix <- function(cM = matrix()) {
	## Cache matrix cM and its inverse.
	
	## Initialize the inverse.
    inv <- NULL
    
	## Use set to set cM.
    set <- function(M) {
        cM  <<- as.matrix(M) # Explicit conversion tot matrix !
        inv <<- NULL
    }
    
	## Use get to get cM.
    get <- function() {
        cM
    }
    
	## Use setInverse to set the inverse of cM.
    setInverse <- function(Minv) {
		inv <<- Minv        
    }
    
	## Use getInverse to get the inverse of cM.
    getInverse <- function() {
        inv
    }
    
	## Return the public methods of makeCacheMatrix (OO-talk).
    list(
        set        = set,
        get        = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## cacheSolve() accepts a makeCacheMatrix matrix and
## looks for a cached inverser If there's no such inverse
## it calculates the inverse using solve(). Finally it
## return the inverse.

cacheSolve <- function(M, ...) {
	## Return a matrix that is the inverse of 'M'
    
    # Try to get the inverse of M
    Minv <- M$getInverse()
    
	## Check existense of inverse and calculates
	## inverse if necesarry.
    if( is.null( Minv ) ) {
        
        message( "No inverse cached, calculating inverse ..." )
        Minv <- solve( M$get() )
        M$setInverse( Minv )
        
    } else {
        
        message( "Retrieved inverse from cache :-)" )
        
    }
    print( Minv )
    
    # Return the inverse
    Minv    
}