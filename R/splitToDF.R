#' Extract fields from a character vector into a dataframe
#' using a regular expression.
#'
#' Read a dataframe from a string vector with regular but Given a
#' regular expression with named fields and a string vector, extract
#' the named fields from each element of the string vector, returning
#' the result as a dataframe.  Each columns is a character vector
#' corresponding to a named field, and each row corresponds to an
#' element of the string vector.
#' 
#' @param rx: Perl-type regular expression with named fields, as described
#' in \code{?regex}
#' 
#' @param s: character vector.  Each element must match \code{rx}, i.e.
#' must have at least one character matching each named field in \code{rx}.
#'
#' @param guess: if \code{TRUE} paste the columns together with
#' commas, and use read.csv to try return the columns already
#' converted to appropriate types, e.g. integer or real. Defaults to
#' \code{TRUE}.
#' 
#' @param ...: additional parameters to \code{read.csv()} used when
#' \code{guess} is \code{TRUE}.
#' 
#' @return a data frame.  Each column is a vector and corresponds to a
#' named field in \code{rx}, going from left to right.  Each item of
#' \code{s} contributes the corresponding row of the return value.  If
#' no items of \code{s} match \code{rx}, the function returns
#' \code{NULL}.  If \code{guess} is \code{TRUE}, columns have been
#' converted to their guessed types.
#'
#' @note: This function serves a similar purpose to \code{read.csv},
#' except that the rules for splitting input lines into columns are
#' much more flexible.  Any format which can be described by a regular
#' expression with named fields can be handled.  For example, logfile
#' messages often contain extra text and variable field positions
#' which prevent direct use of functions like \code{read.csv} or \code{scan}
#' to extract what is really just a dataframe with syntactic sugar.
#'
#' For example, if input lines look like this:
#'
#' s = c( "Mar 10 06:25:11 SG [62442.231077] pps-gpio: PPS @ 1425968711.000018004: pre_age = 163, post_age = 1130",
#'        "Mar 10 06:25:12 SG [62443.2311] pps-gpio: PPS @ 1425968712.000011015: pre_age = 1055, post_age = 11655",
#'        "Mar 10 06:25:13 SG [62444.23] pps-gpio: PPS @ 1425968713.000011275: pre_age = 160, post_age = 12120" )
#' 
#' and we wish to extract timestamps and pre_age and post_age, as a
#' data.frame, we can use this regular expression:
#'
#' rx = ".*pps-gpio: PPS @ (?<ts>[0-9]+\\.[0-9]*): pre_age = (?<pre>[0-9]+), post_age = (?<post>[0-9]+)$"
#'
#' splitToDF(s, rx) then gives:
#' 
#'           ts preAge postAge
#' 1 1425968711  16320   17130
#' 2 1425968712  11055   11655
#' 3 1425968713  11160   12120
#'
#' where the first column is numeric and others are integer.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

splitToDF = function(rx, s, guess=TRUE, ...) {
    ## do the matching
    v = gregexpr(rx, s, perl=TRUE)

    ## trivial result
    if (length(v) == 0)
        return (NULL)

    ## get the names of captured fields
    nm = attr(v[[1]], "capture.names")

    ## allocate a return value list 
    rv = vector("list", length(nm))

    ## get starting positions and lengths for each match in each item
    ## Note that rows correspond to named fields, columns to items of s.
    starts = matrix(sapply(v, function(v) attr(v, "capture.start")), nrow = length(nm))
    lengths = matrix(sapply(v, function(v) attr(v, "capture.length")), nrow = length(nm))

    ## for each named field, extract the matched region of each item of s
    for (i in seq(along=nm))
        rv[[i]] = substring(s, starts[i, ], starts[i, ] + lengths[i, ] - 1)

    if (guess)
        ## guess column types via read.csv()
        rv = read.csv(textConnection(do.call(paste, c(rv, sep=","))), header=FALSE, ...)
    else
        ## preserve columns as strings
        rv = as.data.frame(rv, stringsAsFactors=FALSE)
    
    ## assign column names
    names(rv) = nm
    
    return (rv)
}

