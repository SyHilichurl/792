library(grid)
library(ggplot2)
library(lattice)
library(png)

getGrobInfo <- function(listing) {
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
      alphas <- grob$gp$alpha
      cols <- grob$gp$col
      coords <- grobCoords(grob, closed = TRUE)
      for (j in seq_along(cols)) {
        col <- cols[[j]]
        alpha <- alphas[[j]]
        locs <- deviceLoc(unit(coords[[j]]$x, "in"), unit(coords[[j]]$y, "in"))
        left <- min(locs$x)
        bottom <- min(locs$y)
        right <- max(locs$x)
        top <- max(locs$y)
        if (is.null(alpha))
          alpha <- 1
        if (!is.na(col)) {
          grobInfo[[grobCnt]] <- c(col = col, alpha = alpha,
                                   left = left, bottom = bottom,
                                   right = right, top = top,
                                   index1 = i, index2 = j,
                                   len = length(cols))
          attr(grobInfo[[grobCnt]], "name") <- as.character(gPath)
          grobCnt <- grobCnt + 1
        }
      }
      upViewport(0)
    }
  }
  grobInfo
}

getColInfo <- function(grobInfo) {
  colors <-split(grobInfo,sapply(grobInfo,"[[","col"))
  colInfo <- lapply(colors,
                    function(x) sapply(x,
                                       function(elem) c(name=attr(elem, "name"),elem[2:9])))
  index <- lapply(colInfo, function(x) x[7:8,])
  colInfo2 <- list()
  for (col in names(index)) {
    ind <- index[[col]]
    index1 <- as.numeric(ind[seq(1, length(ind), by = 2)])
    index2 <- as.numeric(ind[seq(2, length(ind), by = 2)])
    combined_index2 <- tapply(index2, index1, FUN = c)
    tmp <- data.frame(index1 = as.numeric(names(combined_index2)),
                                index2 = combined_index2)
    colInfo2[[col]] <- tmp
  }
  colInfo2
}

calColRatio <- function(col,alpha) {
  r <- col2rgb(col)[1,]
  g <- col2rgb(col)[2,]
  b <- col2rgb(col)[3,]
  l <- (0.2126*r + 0.7152*g + 0.0722*b) / 255 * as.numeric(alpha)
  names(l) <- paste0(col, "*", alpha)
  ratio <- outer(l, l, FUN=function(x,y) (x+0.05)/(y+0.05))
  ratio
}

calLumWithAlpah <- function(col_f, alpha_f, col_b, alpha_b) {
  alpha <- alpha_f + alpha_b * (1-alpha_f)
  rgb <- col2rgb(c(col_f, col_b))
  r <- (rgb[1,1]*alpha_f + rgb[1,2]*(1-alpha_f)*alpha_b)/alpha
  g <- (rgb[2,1]*alpha_f + rgb[2,2]*(1-alpha_f)*alpha_b)/alpha
  b <- (rgb[3,1]*alpha_f + rgb[3,2]*(1-alpha_f)*alpha_b)/alpha
  l <- (0.2126*r + 0.7152*g + 0.0722*b) / 255
  names(l) <- NULL
  l
}

calOverlap <- function(grobInfo) {
  n <- length(grobInfo)
  pairs <- list()
  cnt <- 1
  for (i in seq_along(grobInfo[-n])) {
    for (j in seq_along(grobInfo[(i + 1):n])) {
      l1 <- grobInfo[[i]][3]
      t1 <- grobInfo[[i]][4]
      r1 <- grobInfo[[i]][5]
      b1 <- grobInfo[[i]][6]
      l2 <- grobInfo[[i+j]][3]
      t2 <- grobInfo[[i+j]][4]
      r2 <- grobInfo[[i+j]][5]
      b2 <- grobInfo[[i+j]][6]
      if (!(r1<l2 || r2<l1 || b1<t2 || b2<t1 ||
            (l2>=l1 && r2<=r1 && t2<=t1 && b2>=b1) ||
            (l2<=l1 && r2>=r1 && t2>=t1 && b2<=b1))) {
        ratio <- calColRatio(c(grobInfo[[i]][1], grobInfo[[j]][1]),
                        c(grobInfo[[i]][2], grobInfo[[j]][2]))[2]
        if (ratio<4.5 && ratio>1/4.5) {
          pairs[[cnt]] <- c(ratio=ratio, i, j)
          cnt <- cnt + 1
        }
      }
    }
  }
  pairs
}

countFacets <- function(pair, item) {
  cnt=0
  for (i in seq_along(pair)) {
    if (attr(grobInfo[[pair[[i]][2]]], "name") %in% item ||
        attr(grobInfo[[pair[[i]][3]]], "name") %in% item) {
      cnt=cnt+1
    }
  }
  nrow <- n2mfrow(cnt)[1]
  ncol <- n2mfrow(cnt)[2]
  c(nrow, ncol)
}

g2plot <- function(g) {
  UseMethod("g2plot")
}

g2plot.ggplot <- function(g) {
  print(g)
  grid.force()
}

drawMatch <- function(g, grobInfo, pair, item, RandC, facet="page") {
  nrow <- RandC[1]
  ncol <- RandC[2]
  img0 <- readPNG("plot0.png")
  old <- devAskNewPage(TRUE)
  for (i in seq_along(pair)) {
    if (attr(grobInfo[[pair[[i]][2]]], "name") %in% item ||
        attr(grobInfo[[pair[[i]][3]]], "name") %in% item) {
      grid.newpage()
      g
      lty=3
      x = as.numeric(grobInfo[[pair[2]]][3])
      width = as.numeric(grobInfo[[pair[[i]][2]]][5]) -
        as.numeric(grobInfo[[pair[[i]][2]]][3])
      y = as.numeric(grobInfo[[pair[[i]][2]]][4])
      height = as.numeric(grobInfo[[pair[[i]][2]]][6]) -
        as.numeric(grobInfo[[pair[[i]][2]]][4])
      grid.rect(x = x, y = y, width = width, height = height,
                default.units = "in", just = c(0, 0),
                gp = gpar(col = "red", fill = rgb(1,0,0,0.1), lty=lty))
      x = as.numeric(grobInfo[[pair[[i]][3]]][3])
      width = as.numeric(grobInfo[[pair[[i]][3]]][5]) -
        as.numeric(grobInfo[[pair[[i]][3]]][3])
      y = as.numeric(grobInfo[[pair[[i]][3]]][4])
      height = as.numeric(grobInfo[[pair[[i]][3]]][6]) -
        as.numeric(grobInfo[[pair[[i]][3]]][4])
      grid.rect(x = x, y = y, width = width, height = height,
                default.units = "in", just = c(0, 0),
                gp = gpar(col = "red", fill = rgb(1,0,0,0.1), lty=lty))
    }
  }
  devAskNewPage(old)
}


excludeGrob <- function(grobInfo, exclude) {
  findMatch <- function(exclude) {
    sapply(grid.grep(exclude, grep = TRUE, global = TRUE), as.character)
  }
  matches <- unlist(lapply(exclude, findMatch))
  findGrob <- function(g) {
    !attr(g, "name") %in% matches
  }
  Filter(findGrob, grobInfo)
}

includeGrob <- function(grobInfo, include) {
  findMatch <- function(include) {
    sapply(grid.grep(include, grep = TRUE, global = TRUE), as.character)
  }
  matches <- unlist(lapply(include, findMatch))
  findGrob <- function(g) {
    attr(g, "name") %in% matches
  }
  Filter(findGrob, grobInfo)
}

g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
  labs(title = "test") + facet_wrap(~gear)
g2 <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
  scale_color_manual(values = c("1" = "blue", "0" = "azure"))
#png("plot0.png")
#g2plot(g2)
grid.force()
listing <- do.call(cbind, grid.ls(view=TRUE, print=FALSE))
grobInfo <- getGrobInfo(listing)
pair <- calOverlap(grobInfo)
#dev.off()
#img0 <- readPNG("plot0.png")
include <- "."
exclude <- NULL
if (length(include))
  grobInfoF <- includeGrob(grobInfo, include)
if (length(exclude))
  grobInfoF <- excludeGrob(grobInfo, exclude)
item <- sapply(grobInfoF, function(x) attr(x, "name"))
RandC <- countFacets(pair, item)

drawMatch(g2, grobInfo, pair, item, RandC, facet="page")



