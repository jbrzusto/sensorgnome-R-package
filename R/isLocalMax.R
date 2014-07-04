isLocalMax = function(x, y, dx=1) {
    ## Which pairs (x[i], y[i]) are local maxima?
    ##
    ## e.g. if A is a data.frame with columns time and temp
    ## giving temperature at various times, then
    ##
    ##   A[isLocalMax(A$time, A$temp, 10),]
    ##
    ## gives those rows of A which were hotter than any row within
    ## 10 seconds before or after.
    ##
    ## A local maximum is a y value higher than all nearby y values,
    ## where 'nearby' means having x within 'dx' on either side.
    ## i.e. a pair (x[i], y[i]) is *not* a local max if there is another
    ## pair (x[j], y[j]) with y[j] >= y[j] and abs(x[j] - x[i]) <= dx
    ## and i != j.
    ##
    ## e.g.:
    ##                            +y3
    ##                +y2         |
    ##        +y1     |           |     +y4           +y6                
    ##        |       |           |     |        +y5  |                  
    ##        |       |           |     |        |    |                  
    ##        |       |           |     |        |    |                    
    ## -------x1------x2----------x3----x4-------x5---x6---------
    ## -------|<--8-->|<----12--->|<-6->|<---9-->|<-5>|----------
    ##
    ## Here, if dx is 8 and the separations between x values
    ## are as shown, and the y values are as shown, then:
    ## - y1 is not a local max (y2 is larger and 8 units away on the x axis)
    ## - y2 is a local max (larger than y1, and farther than 8 units away on the x axis from y3)
    ## - y3 is a local max (larger than all other y here)
    ## - y4 is not a local max (smaller than y3 which is 6 units away on the x axis)
    ## - y5 is not a local max (smaller than y6 which is 5 units away on the x axis)
    ## - y6 is a local max (larger than y5; nothing to the right within 8 units along x axis)

    ## parameters:
    ##
    ## x: vector of locations on x axis, in increasing order
    ## y: vector of corresponding y values; length (y) = length(x)
    ## dx: maximum distance along x axis to look for 'nearby' points
    ##
    ## returns: 
    ##
    ## rv: boolean vector of same length as x and y.
    ## rv[i] is TRUE if and only if (x[i], y[i]) is a local maximum
    ##

    if (length(x) != length(y))
        stop("Lengths of x and y must be equal")

    if (any(diff(x) <= 0))
        stop("x coordinates must be in increasing order")
    
    ismax = rep(FALSE, length(x))  ## assume no local maxima

    mi = 1     ## index of most recent maximum candidate
    my = y[1]  ## y-coordinate of most recent maximum candidate
    mx = x[1]  ## x-coordinate of most recent maximum candidate
    
    for (i in 2:length(x)) {
        if (x[i] - mx <= dx & y[i] <= my)
            ## this row is too close to the previous
            ## max and is not larger, so skip it
            next
        
        if (x[i] - mx > dx) {
            ## we're far enough away from the last max
            ## that it can be kept
            ismax[mi] = TRUE

        }
        ## either way, this row is now the new candidate for a max
        mi = i
        mx = x[i]
        my = y[i]
    }
    ismax[mi] = TRUE
    return (ismax)
}
