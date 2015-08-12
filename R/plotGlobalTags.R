#' Plot all detections of a set of tags
#'
#' Plots site vs time for each tag, with sites ordered by increasing latitude.
#' Signal strength is superimposed on the curve for each tag.
#'
#' @param x - data.frame of tag detections, as returned by \code{tags}
#'
#' @param tagsPerPanel - number of tags plotted per panel
#'
#' @param panelsPerRow - number of panels to show per row of panels
#' 
#' @param siteLatThreshold - number of sites above which plotting is vs. latitude, not site
#'  
#' @return nothing
#'
#' Plots to the current graphics device.  For plots with many sites or tags,
#' you may want to open a raster plotting device before calling this function;
#' e.g. \code{png("myBigPlot.png", width=1000, height=4000); plotGlobalTags(x); dev.off()}
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

plotGlobalTags = function(x, tagsPerPanel=5, panelsPerRow=3, siteLatThreshold=20) {
    ## condense to at most one record per tag per site per hour

    tsSlot = round(x$ts, "hours")
    dup = duplicated(paste(x$label,x$site,tsSlot))
    ## drop duplicates and bogus dates
    xx = x[!dup & x$ts > ymd("2008-01-01"),]
    xx$lat[xx$lat==999] = NA
    xx$lon[xx$lon==999] = NA

    ## get number of tags
    nt = length(unique(x$label))

    nr = ceiling(nt / (tagsPerPanel * panelsPerRow))

    ## get the mean lat/lon by site
    coords = subset(xx, !is.na(lat), c(site, lat, lon))
    siteLat = with(coords, tapply(lat, site, mean))
    siteLon = with(coords, tapply(lon, site, mean))

    if (length(siteLat) == 0) {
        xx$sitefact = as.factor(xx$site)
    } else {
        siteOrder = order(siteLat)
        ns = length(siteLat)
        xx$sitefact = factor(xx$site, levels=names(siteLat)[siteOrder], ordered=TRUE)
        levels(xx$sitefact) = sprintf("%s (%6.3fN, %6.3fW)", names(siteLat), siteLat, siteLon)[siteOrder]
    }

    ## We want to plot multiple tags per panel, so sort their labels and create a grouping factor
    ## Note that labels are sorted in increasing order by ID
    labs = xx$label[order(xx$id,xx$label)]
    dup = duplicated(labs)
    tagLabs = labs[!dup]
    tagGroupIDs = xx$id[order(xx$id,xx$label)][!dup]
    tagGroup = 1 + floor((0:length(tagLabs)) / tagsPerPanel)
    ngroup = length(tagGroup)
    names(tagGroup) = tagLabs
    tagGroupFactor = tagGroup[as.character(xx$label)]
    tagGroupLabels = tapply(tagGroupIDs, 1 + floor((0:(length(tagGroupIDs)-1)) / tagsPerPanel), function(x) paste("IDs:", paste(sort(unique(x)), collapse=",")))
    xx$tagGroupFactor = factor(tagGroupFactor, labels=tagGroupLabels, ordered=TRUE)

    dbScale = if(length(levels(xx$sitefact)) > 2) 150 else 1000
    
    ## colour for grid lines
    gridColour = "#00000040"

    ## plotting character
    plotChar = 4
    
    if (length(siteLat) <= siteLatThreshold) {
        ## if siteLatThreshold sites or fewer, plot by site
        xyplot(sitefact~ts|tagGroupFactor, groups=label, data=xx, type="b", pch=plotChar,
               layout = c(panelsPerRow, nr),
               panel = function(x, y, groups, subscripts, ...) {
                   ## ugly code to eliminate factor levels other than those in this panel
                   groups = as.factor(as.character(groups[subscripts]))
                   subscripts = 1:length(subscripts)
                   panel.abline(h=1:length(levels(y)), col = gridColour)
                   daySeq = seq(from = round(min(xx$ts), "day"), to=round(max(xx$ts)), by=5 * 24 * 3600)
                   panel.abline(v=daySeq, col = gridColour)
                   key = simpleKey(levels(groups), cex=0.8, just=c(0, 0))
                   draw.key(key, TRUE)
                   panel.xyplot(x, as.numeric(y) + (xx$dbm[subscripts] + 100)/dbScale, groups=groups, subscripts=subscripts, ...)
               },
               main="Detection Site vs. Time by Tag",
               sub="Signal strength is superimposed on curves at each site",
               xlab="Date (GMT)",
               ylab="Site",
               )
    } else {
        ## > siteLatThreshold sites, plot by latitude, don't group
        xyplot(lat~ts|tagGroupFactor, groups=label, data=xx, type="b", pch=plotChar, lty=3,
               layout = c(panelsPerRow, nr),
               panel = function(x, y, groups, subscripts, ...) {
                   ## ugly code to eliminate factor levels other than those in this panel
                   groups = as.factor(as.character(groups[subscripts]))
                   subscripts = 1:length(subscripts)
                   panel.abline(h=seq(from = round(min(xx$lat, na.rm=TRUE)), to=round(max(xx$lat, na.rm=TRUE)), by=1), col = gridColour)
                   daySeq = seq(from = round(min(xx$ts), "day"), to=round(max(xx$ts)), by=5 * 24 * 3600)
                   panel.abline(v=daySeq, col = gridColour)
                   key = simpleKey(levels(groups), cex=0.8, just=c(0, 0))
                   draw.key(key, TRUE)
                   panel.xyplot(x, as.numeric(y) + (xx$dbm[subscripts] + 100)/dbScale, groups=groups, subscripts=subscripts, ...)
               },
               main="Detection Latitude vs. Time by Tag",
               sub="Signal strength is superimposed on curves, adding ~ +/- 0.5 degrees latitude",
               xlab="Date (GMT)",
               ylab="Latitude (degrees North)"
               )
    }
}
    
