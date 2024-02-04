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


drawNotAligned <- function(notAlignInfo, listing, item) {
  if (length(notAlignInfo) != 0) {
    cat("\nNot Aligned!!\n")
    for (i in 1:(length(notAlignInfo)/3)) {
      gPath <- gPath(listing[notAlignInfo[1, i], "gPath"],
                     listing[notAlignInfo[1, i], "name"])
      vPath <- listing[notAlignInfo[1, i], "vpPath"]
      if (as.character(gPath) %in% item) {
        cat(as.character(gPath), "-", notAlignInfo[2, i], "\n")
        editGrobIndex(gPath, vPath, notAlignInfo[2, i], notAlignInfo[3, i])
      }
    }
  }
}



nameFour <- function(x, rounding) {
  x_str <- as.character(round(x,rounding-1))
  x_str <- gsub("\\.", "", x_str)
  result <- as.character(substr(x_str, 1, rounding))
  result
}

countFacets <- function(matchInfo, grobInfo, item, r, align) {
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
  if (align=="b" | align=="v") {
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
  cat("\nFacets ")
  cat(grDevices::n2mfrow(cnt))
  nrow <- n2mfrow(cnt)[1]
  ncol <- n2mfrow(cnt)[2]
  c(nrow, ncol)
}


drawMatch <- function(matchInfo, grobInfo, item, r, align, RandC) {
  # findMatch <- function(item) {
  #   sapply(grid.grep(item, grep = TRUE, global = TRUE), as.character)
  # }
  # matches <- unlist(lapply(item, findMatch))
  nrow <- RandC[1]
  ncol <- RandC[2]
  vps <- viewport(width = ncol*grobInfo[[1]]["right"], height = nrow*grobInfo[[1]]["top"],
                  default.units = "inch",
                  layout = grid.layout(nrow, ncol))
  pushViewport(vps)
  j = 1

  matches <- logical(length(item))
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
        crow <- ceiling(j / ncol)
        ccol <- j %% ncol
        ccol[ccol == 0] <- ncol
        cvp = viewport(layout.pos.row = crow, layout.pos.col = ccol)
        print(g, vp=cvp)
        j = j + 1
        for (i in matchInfo$xAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
          }
          x = grobInfo[[i]][1]
          width = (grobInfo[[i]][2] - grobInfo[[i]][1])
          y = grobInfo[[i]][3]
          height = (grobInfo[[i]][4] - grobInfo[[i]][3])
          # x = grobInfo[[i]][1]/ncol
          # width = (grobInfo[[i]][2] - grobInfo[[i]][1])/ncol
          # y = grobInfo[[i]][3]/nrow
          # height = (grobInfo[[i]][4] - grobInfo[[i]][3])/nrow
          name = paste0("x.", nameFour(x,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name, vp=cvp)
          cnt = cnt + 1
        }
        grid.lines(x = matchInfo$x[index], default.units = "in",
                   gp = gpar(col = "blue"), vp=cvp,
                   name = paste0("x.", nameFour(matchInfo$x[index],r), ".alignment"))
        # grid.lines(x = matchInfo$x[index]/ncol, default.units = "in",
        #            gp = gpar(col = "blue"), vp=cvp,
        #            name = paste0("x.", nameFour(matchInfo$x[index],r), ".alignment"))
      }
    }
  }
  if (align=="b" | align=="v") {
    for (index in seq_along(matchInfo$y)) {
      flag = FALSE
      cnt = 1
      for (i in matchInfo$yAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        crow <- ceiling(j / ncol)
        ccol <- j %% ncol
        ccol[ccol == 0] <- ncol
        cvp = viewport(layout.pos.row = crow, layout.pos.col = ccol)
        grid.rect(vp=cvp)
        print(g, vp=cvp)
        j = j + 1
        for (i in matchInfo$yAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
          }
          x = grobInfo[[i]][1]
          width = (grobInfo[[i]][2] - grobInfo[[i]][1])
          y = grobInfo[[i]][3]
          height = (grobInfo[[i]][4] - grobInfo[[i]][3])
          # x = grobInfo[[i]][1]/ncol
          # width = (grobInfo[[i]][2] - grobInfo[[i]][1])/ncol
          # y = grobInfo[[i]][3]/nrow
          # height = (grobInfo[[i]][4] - grobInfo[[i]][3])/nrow
          name = paste0("y.", nameFour(y,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name, vp=cvp)
          cnt = cnt + 1
        }
        grid.lines(y = matchInfo$y[index], default.units = "in",
                   gp = gpar(col = "blue"), vp=cvp,
                   name = paste0("y.", nameFour(matchInfo$y[index],r), ".alignment"))
        # grid.lines(y = matchInfo$y[index]/nrow, default.units = "in",
        #            gp = gpar(col = "blue"), vp=cvp,
        #            name = paste0("y.", nameFour(matchInfo$y[index],r), ".alignment"))
      }
    }
  }
  upViewport()
  if (length(item[matches])>0)
    cat("\nAligned!!\n")
  for (i in seq_along(item[matches]))
    cat(item[matches][i], "\n")
  item[matches]
}

