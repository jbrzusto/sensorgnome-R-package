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
    ## x: vector of locations on x axis, in non-decreasing order
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

    if (any(diff(x) < 0))
        stop("x coordinates must be in non-decreasing order")

    ## We make two passes, one left to right, and one right to left.
    ## A point is a local maximum iff it succeeds on each pass.
    ## Success on a single pass is achieved iff we reach dx past the
    ## point (or the end of the list of points) without finding a
    ## higher point.

    ## Each pass starts with one end point as a candidate for local
    ## max, and examines consecutive points (in the pass direction)
    ## one at a time.
    ##
    ## One of four conditions holds:
    ##
    ## a) the examined point is <= dx away, and less than the candidate.
    ##    Continue examining points, leaving the candidate as-is.
    ##
    ## b) the examined point is more than dx away from the candidate.
    ##    The candidate succeeds.  The examined point is the new candidate.
    ##
    ## c) the examined point is <= dx away from the candidate but
    ##    not less than the candidate.
    ##    The candidate fails.  The examined point is the new candidate.
    ##
    ## d) the end of the list of points has been reached.
    ##    The candidate succeeds.
    ##
    ## In cases b and c, we implicitly fail any points between the
    ## examined point and the and the (old) candidate (aka "intervening
    ## points") because:
    ##
    ## - in case b, the intervening points are within dx of the candidate
    ##   and smaller than it
    ##
    ## - in case c, the intervening points are within dx of the examined
    ##   point and smaller than it.
    ##
    ## This is an optimization which obviates having to try each point
    ## as a candidate, since there's no reason to let a point succeed
    ## a pass if we know it fails the opposite pass.

    s1 = s2 = rep(FALSE, length(x))  ## success on passes 1 and 2

    mi = 1     ## index of most recent candidate
    my = y[1]  ## y-coordinate of most recent candidate
    mx = x[1]  ## x-coordinate of most recent candidate

    ## forward pass:
    
    for (i in 2:length(x)) {
        if (x[i] - mx <= dx && y[i] < my)
            ## case a)
            ## this point is a neighbour of the candidate
            ## but is smaller, so it doesn't disqualify
            ## the candidate
            next
        
        if (x[i] - mx > dx) {
            ## case b)
            ## we're far enough away from the candidate
            ## so it succeeds
            s1[mi] = TRUE
        }
        ## case c: s1[mi] = FALSE, but it's initialized that way
        
        ## cases b, c: make this point the new candidate
        mi = i
        mx = x[i]
        my = y[i]
    }
    ## case d: the candidate passes
    s1[mi] = TRUE

    ## reverse pass:

    mi = length(x)
    mx = x[mi]
    my = y[mi]
    for (i in (length(x) - 1):1) {
        if (mx - x[i] <= dx && y[i] < my)
            ## case a)
            ## this point is a neighbour of the candidate
            ## but is smaller, so it doesn't disqualify
            ## the candidate
            next
        
        if (mx - x[i] > dx) {
            ## case b)
            ## we're far enough away from the candidate
            ## so it succeeds
            s2[mi] = TRUE
        }
        ## case c: s2[mi] = FALSE, but it's initialized that way
        
        ## cases b, c: make this point the new candidate
        mi = i
        mx = x[i]
        my = y[i]
    }
    ## case d: the candidate passes
    s2[mi] = TRUE

    return (s1 & s2)
}
