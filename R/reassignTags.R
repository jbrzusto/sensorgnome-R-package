#' Reassign tags to a different project.
#' 
#' Given a set of tag fullIDs, reassign them to a different project.  This
#' changes the project designation in both the registration and detection
#' databases.
#' 
#' @param fullID character, fullID of tag, as it occurs in the master database
#' 
#' @param to character, project code, one of the items returned by getProjects()
#'
#' @param year integer, year of project registration; defaults to current field season
#' (current year minus 3 months)
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
#'

reassignTags = function(fullID, to, year = lubridate::year(Sys.time() - 3*30*24*3600)) {
    projs = getProjects(year)
    if (! to %in% projs)
        stop("Destination project 'to' must a project codes for the specified year, as returned by getProjects()")
    
    rt = getRegTags(year)

    ## get tag records matching source specification, if any
    rt.fullID = makeFullID(rt)

    tfrom = match(fullID, rt.fullID)
    if (any(is.na(tfrom))) {
        stop("These fullIDs don't match any registration database entries: \n", paste(fullID[is.na(tfrom)], collapse=", "))
    }

    ## change project code in tag finder DB
    rt$proj[tfrom] = to

    putRegTags(rt, year)

    ## change tag fullID in tag deployment DB

    tagDeps = read.csv(sprintf("/SG/%d_tag_deployments.csv", year), as.is=TRUE)

    tdfrom = match(fullID, tagDeps$fullID)
    if (any(is.na(tdfrom))) {
        stop("Couldn't find a tag deployment record for tags:\n", paste(fullID[is.na(tdfrom)], collapse=", "))
    }

    ## change project code in tag deployments DB
    tagDeps$proj[tdfrom] = to

    ## as of 2014, we must change the following fields in the corresponding tag records
    ## fullID, label, tagProj

    newFullID = makeFullID(rt[tfrom,])

    ## change fullID in tagDeployments DB
    tagDeps$fullID[tdfrom] = newFullID
    tagDeps = tagDeps[order(tagDeps$fullID),]
    
    write.csv(tagDeps,sprintf("/SG/%d_tag_deployments.csv", year), row.names=FALSE)

    ## new labels are:
    newLabel = getLabel(newFullID, year)$label

    ## run a query for each tag ID on the sqlite database

    con = dbConnect(RSQLite::SQLite(), sprintf("/SG/%d_alltags.sqlite", year))

    sql = function(...) dbGetQuery(con, sprintf(...))

    for (i in seq(along=fullID)) {
        sql("update tags set fullID='%s', label='%s', tagProj='%s' where fullID='%s'",
            newFullID[i],
            newLabel[i],
            to,
            fullID[i]
            )
    }
    dbDisconnect(con)
    return(invisible(TRUE))
}
    


