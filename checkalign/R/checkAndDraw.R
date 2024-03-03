calcInfo <- function(listing, separateText, rounding=4) {
  grobInfo <- getGrobInfo(listing, separateText)
  boundsInfo <- getBoundsInfo(grobInfo, rounding)
  notAlignInfo <- checkNotAligned(grobInfo, boundsInfo)
  matchInfo <- matchAlignment(boundsInfo)
  info <- list(grobInfo = grobInfo, boundsInfo = boundsInfo,
               notAlignInfo = notAlignInfo, matchInfo = matchInfo)
  attr(info, "rounding") <- attr(boundsInfo, "rounding")
  info
}


drawAlignment <- function(g, listing, info, show = "both", showInOne=FALSE,
                          align = "b", include=".", exclude=NULL, rounding=4) {
  grobInfoF <- info$grobInfo
  if (length(include))
    grobInfoF <- includeGrob(grobInfoF, include)
  if (length(exclude))
    grobInfoF <- excludeGrob(grobInfoF, exclude)
  item <- sapply(grobInfoF, function(x) attr(x, "name"))
  res1 <- 0
  res2 <- 0
  if(show == "unaligned" | show == "both") {
    png("plot_unaligned.png", width=400, height=400)
    g2plot(g)
    listing_new <- do.call(cbind, grid.ls(view=TRUE))
    grid.rect(gp = gpar(fill = rgb(1,1,1,0.7), col=NA), name = "shade.highlight")
    res1 <- drawNotAligned(info$notAlignInfo, listing, listing_new, item)
    dev.off()
  }
  if(show == "aligned" | show == "both") {
    png("plot_aligned.png", width=400, height=400)
    RandC <- countFacets(info$matchInfo, info$grobInfo, item, rounding, align)
    res2 <- drawMatch(g, info$matchInfo, info$grobInfo, item, rounding, align, RandC, showInOne)
    dev.off()
  }
  list("unaligned" = res1, "aligned" = as.list(table(res2)))
}


checkAndDraw <- function(g, show, showInOne, align, include, exclude,
                         separateText, rounding) {
  listing <- do.call(cbind, grid.ls(view=TRUE))
  info <- calcInfo(listing, separateText, rounding)
  dev.off()
  rounding <- attr(info, "rounding")
  res <- drawAlignment(g, listing, info, show, showInOne, align,
                include, exclude, rounding)
  grid.newpage()
  if (show == "unaligned") {
    p1 <- readPNG("plot_unaligned.png")
    grid.raster(p1)
  } else if (show == "aligned") {
    p2 <- readPNG("plot_aligned.png")
    grid.raster(p2)
  } else {
    p1 <- readPNG("plot_unaligned.png")
    p2 <- readPNG("plot_aligned.png")
    pushViewport(viewport(x=0, width=.5, just="left"))
    grid.raster(p1)
    popViewport()
    pushViewport(viewport(x=.5, width=.5, just="left"))
    grid.raster(p2)
    popViewport()
  }
  unlink("plot_unaligned.png")
  unlink("plot_aligned.png")
  res
}
