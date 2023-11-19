#' Title
#'
#' @param listing
#' @param separateText
#'
#' @return l,r,b,t,index1,index2,len
#' @export
#'
#' @examples
getGrobInfo <- function(listing, separateText) {
  grobInfo <- list()
  grobCnt <- 1
  for (i in 1:nrow(listing)) {
    if (listing[i, "type"] == "grobListing") {
      gPath <- gPath(listing[i, "gPath"], listing[i, "name"])
      # cat(as.character(gPath), "\n")
      vPath <- listing[i, "vpPath"]
      grob <- grid.get(gPath)
      if (vPath != "ROOT")
        downViewport(gsub("ROOT::", "", vPath))
      if (separateText) {
        if ("text" %in% class(grob))
          coords <- textCoords(grob)
        else
          coords <- grobCoords(grid.get(gPath), closed = TRUE)
      } else
        coords <- grobCoords(grid.get(gPath), closed = TRUE)
      for (j in seq_along(coords)) {
        locs <- deviceLoc(unit(coords[[j]]$x, "in"), unit(coords[[j]]$y, "in"))
        left <- min(locs$x)
        bottom <- min(locs$y)
        right <- max(locs$x)
        top <- max(locs$y)
        grobInfo[[grobCnt]] <- c(left = left, right = right,
                                 bottom = bottom, top = top,
                                 index1 = i, index2 = j,
                                 len = length(coords))
        attr(grobInfo[[grobCnt]], "name") <- as.character(gPath)
        grobCnt <- grobCnt + 1
      }
      upViewport(0)
    }
  }
  grobInfo
}

#' Title
#'
#' @param grobInfo
#' @param index
#' @param rounding
#'
#' @return
#'
#' @examples
getBounds <- function(grobInfo, index, rounding = 4) {
  if (length(grobInfo)>0) {
    bounds <- round(sapply(grobInfo, function(x) x[index]),
                    digits = rounding)
    dup <- duplicated(bounds)
    dup2 <- duplicated(bounds, fromLast = TRUE)
    boundsUni <- bounds[which(!dup & !dup2)]
    list(bounds = bounds, dup = dup, boundsUni = boundsUni)
  }
}

#' Title
#'
#' @param grobInfo
#' @param rounding
#'
#' @return
#'
#' @examples
getBoundsInfo <- function(grobInfo, rounding = 4){
  leftBounds <- getBounds(grobInfo, 1, rounding)
  rightBounds <- getBounds(grobInfo, 2, rounding)
  bottomBounds <- getBounds(grobInfo, 3, rounding)
  topBounds <- getBounds(grobInfo, 4, rounding)
  boundsInfo <- list(left = leftBounds, right = rightBounds,
                     bottom = bottomBounds, top = topBounds)
  attr(boundsInfo, "rounding") <- rounding
  boundsInfo
}

#' Title
#'
#' @param grobInfo
#' @param boundsInfo
#'
#' @return
#'
#' @examples
checkNotAligned <- function(grobInfo, boundsInfo) {
  l <- which(boundsInfo$left$bounds %in%
               setdiff(boundsInfo$left$boundsUni, boundsInfo$right$boundsUni))
  r <- which(boundsInfo$right$bounds %in%
               setdiff(boundsInfo$right$boundsUni, boundsInfo$left$boundsUni))
  b <- which(boundsInfo$bottom$bounds %in%
               setdiff(boundsInfo$bottom$boundsUni, boundsInfo$top$boundsUni))
  t <- which(boundsInfo$top$bounds %in%
               setdiff(boundsInfo$top$boundsUni, boundsInfo$bottom$boundsUni))
  notAlignGrobIndex <- Reduce(intersect, list(l, r, b, t))
  notAlignInfo <- sapply(grobInfo[notAlignGrobIndex], function(x) x[c(5, 6, 7)])
  notAlignInfo
}

#' Title
#'
#' @param boundsInfo
#'
#' @return
#'
#' @examples
matchAlignment <- function(boundsInfo) {
  x <- unique(c(boundsInfo[[1]]$bounds[boundsInfo[[1]]$dup],
                boundsInfo[[2]]$bounds[boundsInfo[[2]]$dup]))
  xAlignment <- vector("list", length(x))
  for (i in seq_along(x)) {
    matching <- which(boundsInfo$left$bounds == x[i] |
                        boundsInfo$right$bounds == x[i])
    xAlignment[[i]] <- unname(matching)
  }
  y <- unique(c(boundsInfo[[3]]$bounds[boundsInfo[[3]]$dup],
                boundsInfo[[4]]$bounds[boundsInfo[[4]]$dup]))
  yAlignment <- vector("list", length(y))
  for (i in seq_along(y)) {
    matching <- which(boundsInfo$bottom$bounds == y[i] |
                        boundsInfo$top$bounds == y[i])
    yAlignment[[i]] <- unname(matching)
  }
  list(x = x, y = y, xAlignment = xAlignment, yAlignment = yAlignment)
}

