#' Title
#'
#' @param listing
#' @param separateText
#' @param rounding
#'
#' @return
#' @importFrom getInfo
#' @examples
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

#' Title
#'
#' @param listing
#' @param info
#' @param show
#' @param include
#' @param exclude
#' @param rounding
#'
#' @return
#' @importFrom drawGrob
#' @examples
drawAlignment <- function(listing, info, show = "both", align = .5,
                          include=".", exclude=NULL, rounding=4) {
  grobInfoF <- info$grobInfo
  if (length(include))
    grobInfoF <- includeGrob(grobInfoF, include)
  if (length(exclude))
    grobInfoF <- excludeGrob(grobInfoF, exclude)
  item <- sapply(grobInfoF, function(x) attr(x, "name"))

  grid.rect(gp = gpar(fill = rgb(1,1,1,0.7), col=NA), name = "shade.highlight")
  if(show == "unaligned" | show == "both")
    drawNotAligned(info$notAlignInfo, listing, item)
  if(show == "aligned" | show == "both") {
    #   drawSameSide(info$boundsInfo) # e.g. left & left
    #   drawDiffSide(info$boundsInfo) # e.g. left & right
    # }
    res <- drawMatch(info$matchInfo, info$grobInfo, item, rounding, align)
    table(res)
  }
}

#' Title
#'
#' @param show
#' @param include
#' @param exclude
#' @param separateText
#' @param rounding
#'
#' @return
#' @export
#'
#' @examples
checkAndDraw <- function(show, align, include, exclude, separateText, rounding) {
  listing <- do.call(cbind, grid.ls(view=TRUE))
  info <- calcInfo(listing, separateText, rounding)
  rounding <- attr(info, "rounding")
  drawAlignment(listing, info, show, align,
                include, exclude, rounding)
}
