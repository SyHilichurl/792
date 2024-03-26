excludeGrob <- function(grobInfo, exclude) {
  exclude <- paste(exclude, collapse = "|")
  findGrob <- function(g) {
    # !attr(g, "name") %in% matches
    !grepl(exclude, attr(g, "name"))
  }
  Filter(findGrob, grobInfo)
}


includeGrob <- function(grobInfo, include) {
  include <- paste(include, collapse = "|")
  findGrob <- function(g) {
    # attr(g, "name") %in% matches
    grepl(include, attr(g, "name"))
  }
  Filter(findGrob, grobInfo)
}


editGrobIndex <- function(gPath, vPath, index, len) {
  name <- paste0(as.character(gPath), ".", as.character(index), ".highlight")
  grob2 <- grid.get(gPath)
  grob2$name <- name
  col = rep(rgb(0,0,0,0), len)
  col[index] = "red"
    if (vPath != "ROOT")
      downViewport(gsub("ROOT::", "", vPath))
  grob2 <- editGrob(grob2, gp = gpar(col = col, fill = NA))
  grid.draw(grob2)
  upViewport(0)
}


drawNotAligned <- function(notAlignInfo, listing, listing_new, item) {
  res <- 0
  if (length(notAlignInfo) != 0) {
    # cat("\nNot Aligned!!\n")
    res <- character(length(notAlignInfo)/3)
    k <- 0
    for (i in 1:(length(notAlignInfo)/3)) {
      gPath <- gPath(listing[notAlignInfo[1, i], "gPath"],
                     listing[notAlignInfo[1, i], "name"])
      #vPath <- listing[notAlignInfo[1, i], "vpPath"]
      if (as.character(gPath) %in% item) {
        gPath <- gPath(listing_new[notAlignInfo[1, i], "gPath"],
                       listing_new[notAlignInfo[1, i], "name"])
        vPath <- listing_new[notAlignInfo[1, i], "vpPath"]
        k <- k + 1
        res[k] <- paste(as.character(gPath), "-", notAlignInfo[2, i])
        # cat(res[k], "\n")
        editGrobIndex(gPath, vPath, notAlignInfo[2, i], notAlignInfo[3, i])
      }
    }
  }
  res[1:k]
}



nameFour <- function(x, rounding) {
  x_str <- as.character(round(x,rounding-1))
  x_str <- gsub("\\.", "", x_str)
  result <- as.character(substr(x_str, 1, rounding))
  result
}

countFacets <- function(matchInfo, grobInfo, item, r, align, facet) {
  cnt=0
  if (align=="b" | align=="v") {
    for (index in seq_along(matchInfo$x)) {
      flag = FALSE
      for (i in matchInfo$xAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        cnt=cnt+1
      }
    }
  }
  if (align=="b" | align=="h") {
    for (index in seq_along(matchInfo$y)) {
      flag = FALSE
      for (i in matchInfo$yAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        cnt=cnt+1
      }
    }
  }
  if (facet == "on")
    cnt = cnt + 1
  # cat("\nFacets ")
  # cat(grDevices::n2mfrow(cnt))
  nrow <- n2mfrow(cnt)[1]
  ncol <- n2mfrow(cnt)[2]
  c(nrow, ncol)
}


drawMatch <- function(g, matchInfo, grobInfo, item, r, align, RandC, facet, show) {
  nrow <- RandC[1]
  ncol <- RandC[2]
  if (nrow*ncol > 25 | nrow*ncol == 0
      | facet == "off" | facet == "page") {
    if (nrow*ncol>25)
      warning("Too many facets for this check!")
    nrow <- 1
    ncol <- 1
    cvp <- viewport(width = grobInfo[[1]]["right"], height = grobInfo[[1]]["top"],
                    default.units = "inch")
    if (facet == "on")
      g2plot(g)
  }
  if (facet != "page") {
    vps <- viewport(layout = grid.layout(nrow, ncol))
    pushViewport(vps)
    j = 1
  }
  img0 <- readPNG("plot0.png")
  matches <- logical(length(item))
  if (facet == "on" && nrow*ncol != 1 && show == "both") {
    crow <- ceiling(j / ncol)
    ccol <- j %% ncol
    ccol[ccol == 0] <- ncol
    cvp = viewport(layout.pos.row = crow, layout.pos.col = ccol)
    vp_width <- convertWidth(unit(cvp$width, "mm"), "in", valueOnly = TRUE)
    vp_height <- convertHeight(unit(cvp$height, "mm"), "in", valueOnly = TRUE)
    img1 <- readPNG("plot_unaligned.png")
    grid.raster(img1,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)
    j = j + 1
  }
  if (align=="b" | align=="v") {
    for (index in seq_along(matchInfo$x)) {
      flag = FALSE
      cnt = 1
      for (i in matchInfo$xAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        if (facet != "page" && nrow*ncol != 1) {
          crow <- ceiling(j / ncol)
          ccol <- j %% ncol
          ccol[ccol == 0] <- ncol
          cvp = viewport(layout.pos.row = crow, layout.pos.col = ccol)
          if (facet == "on")
            grid.raster(img0,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)
          j = j + 1
        }
        if (facet == "page") {
          grid.newpage()
          grid.raster(img0,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)
        }
        title <- ""
        for (i in matchInfo$xAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
            if (title=="") title <- tmp
          }
          x = grobInfo[[i]][1]/ncol
          width = (grobInfo[[i]][2] - grobInfo[[i]][1])/ncol
          y = grobInfo[[i]][3]/nrow
          height = (grobInfo[[i]][4] - grobInfo[[i]][3])/nrow
          name = paste0("x.", nameFour(x,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name, vp=cvp)
          cnt = cnt + 1
        }
        title <- substr(title,
                        gregexpr("::",title)[[1]][length(gregexpr("::",title)[[1]])]+2,
                        nchar(title))
        grid.text(title, vp=cvp,
                  gp=gpar(col="green", cex=1))
        grid.lines(x = matchInfo$x[index]/ncol, default.units = "in",
                   gp = gpar(col = "blue"), vp=cvp,
                   name = paste0("x.", nameFour(matchInfo$x[index],r), ".alignment"))
      }
    }
  }
  if (align=="b" | align=="h") {
    for (index in seq_along(matchInfo$y)) {
      flag = FALSE
      cnt = 1
      for (i in matchInfo$yAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        if (facet != "page" && nrow*ncol != 1) {
          crow <- ceiling(j / ncol)
          ccol <- j %% ncol
          ccol[ccol == 0] <- ncol
          cvp = viewport(layout.pos.row = crow, layout.pos.col = ccol)
          vp_width <- convertWidth(unit(cvp$width, "mm"), "in", valueOnly = TRUE)
          vp_height <- convertHeight(unit(cvp$height, "mm"), "in", valueOnly = TRUE)
          if (facet == "on")
            grid.raster(img0,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)
          j = j + 1
        }
        if (facet == "page") {
          grid.newpage()
          grid.raster(img0,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)
        }
        title <- ""
        for (i in matchInfo$yAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
            if (title == "") title <- tmp
          }
          x = grobInfo[[i]][1]/ncol
          width = (grobInfo[[i]][2] - grobInfo[[i]][1])/ncol
          y = grobInfo[[i]][3]/nrow
          height = (grobInfo[[i]][4] - grobInfo[[i]][3])/nrow
          name = paste0("y.", nameFour(y,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name, vp=cvp)
          cnt = cnt + 1
        }
        #if (gregexpr("::", title)[[1]] != -1)
        title <- substr(title,
                      gregexpr("::",title)[[1]][length(gregexpr("::",title)[[1]])]+2,
                          nchar(title))
        grid.text(title, vp=cvp,
                  gp=gpar(col="green", cex=1))
        grid.lines(y = matchInfo$y[index]/nrow, default.units = "in",
                   gp = gpar(col = "blue"), vp=cvp,
                   name = paste0("y.", nameFour(matchInfo$y[index],r), ".alignment"))
      }
    }
  }
  upViewport(0)
  # if (length(item[matches])>0)
  #   cat("\nAligned!!\n")
  # for (i in seq_along(item[matches]))
  #   cat(item[matches][i], "\n")
  item[matches]
}

