#' @importFrom getInfo
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


#' @importFrom drawGrob
drawAlignment <- function(g, listing, info, show = "both", align = "b",
                          include=".", exclude=NULL, rounding=4) {
  grobInfoF <- info$grobInfo
  if (length(include))
    grobInfoF <- includeGrob(grobInfoF, include)
  if (length(exclude))
    grobInfoF <- excludeGrob(grobInfoF, exclude)
  item <- sapply(grobInfoF, function(x) attr(x, "name"))

  grid.rect(gp = gpar(fill = rgb(1,1,1,0.7), col=NA), name = "shade.highlight")
  if(show == "unaligned" | show == "both") {
    drawNotAligned(info$notAlignInfo, listing, item)
    dev.off()
  }
  if(show == "aligned" | show == "both") {
    png("plot2.png")
    RandC <- countFacets(info$matchInfo, info$grobInfo, item, rounding, align)
    res <- drawMatch(g, info$matchInfo, info$grobInfo, item, rounding, align, RandC)
    table(res)
    dev.off()
  }
}


checkAndDraw <- function(g, show, align, include, exclude, separateText, rounding) {
  listing <- do.call(cbind, grid.ls(view=TRUE))
  info <- calcInfo(listing, separateText, rounding)
  rounding <- attr(info, "rounding")
  drawAlignment(g, listing, info, show, align,
                include, exclude, rounding)
  grid.newpage()
  if (show == "unaligned") {
    p1 <- readPNG("plot1.png")
    grid.raster(p1)
  } else if (show == "aligned") {
    p2 <- readPNG("plot2.png")
    grid.raster(p2)
  } else {
    p1 <- readPNG("plot1.png")
    p2 <- readPNG("plot2.png")
    pushViewport(viewport(x=0, width=.5, just="left"))
    grid.raster(p1)
    popViewport()
    pushViewport(viewport(x=.5, width=.5, just="left"))
    grid.raster(p2)
    popViewport()
  }
  unlink("plot1.png")
  unlink("plot2.png")
}
