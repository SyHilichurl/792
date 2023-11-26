#' Title
#'
#' @param grobInfo
#' @param exclude
#'
#' @return
#'
#' @examples
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

#' Title
#'
#' @param grobInfo
#' @param include
#'
#' @return
#'
#' @examples
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

#' Title
#'
#' @param gPath
#' @param vPath
#' @param index
#' @param len
#'
#' @return
#'
#' @examples
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

#' Title
#'
#' @param notAlignInfo
#' @param listing
#' @param item
#'
#' @return
#'
#' @examples
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


#' Title
#'
#' @param x
#' @param rounding
#'
#' @return
#'
#' @examples
nameFour <- function(x, rounding) {
  x_str <- as.character(round(x,rounding-1))
  x_str <- gsub("\\.", "", x_str)
  result <- as.character(substr(x_str, 1, rounding))
  result
}

#' Title
#'
#' @param matchInfo
#' @param grobInfo
#' @param item
#' @param r
#'
#' @return
#'
#' @examples
drawMatch <- function(matchInfo, grobInfo, item, r, align) {
  # findMatch <- function(item) {
  #   sapply(grid.grep(item, grep = TRUE, global = TRUE), as.character)
  # }
  # matches <- unlist(lapply(item, findMatch))
  matches <- logical(length(item))
  if (align==0.5 | align==0) {
    for (index in seq_along(matchInfo$x)) {
      flag = FALSE
      cnt = 1
      for (i in matchInfo$xAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        for (i in matchInfo$xAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
          }
          x = grobInfo[[i]][1]
          width = grobInfo[[i]][2] - grobInfo[[i]][1]
          y = grobInfo[[i]][3]
          height = grobInfo[[i]][4] - grobInfo[[i]][3]
          name = paste0("x.", nameFour(x,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name)
          cnt = cnt + 1
        }
        grid.lines(x = matchInfo$x[index], default.units = "in",
                   gp = gpar(col = "blue"),
                   name = paste0("x.", nameFour(matchInfo$x[index],r), ".alignment"))
      }
    }
  }
  if (align==0.5 | align==1) {
    for (index in seq_along(matchInfo$y)) {
      flag = FALSE
      cnt = 1
      for (i in matchInfo$yAlignment[[index]])
        if (attr(grobInfo[[i]], "name") %in% item) {
          flag = TRUE
          break
        }
      if (flag) {
        for (i in matchInfo$yAlignment[[index]]) {
          lty = 3
          tmp <- attr(grobInfo[[i]], "name")
          if (tmp %in% item) {
            lty = 1
            matches[which(item==tmp)] <- TRUE
          }
          x = grobInfo[[i]][1]
          width = grobInfo[[i]][2] - grobInfo[[i]][1]
          y = grobInfo[[i]][3]
          height = grobInfo[[i]][4] - grobInfo[[i]][3]
          name = paste0("y.", nameFour(y,r), ".", cnt, ".",
                        attr(grobInfo[[i]],"name"),".alignment")
          grid.rect(x = x, y = y, width = width, height = height,
                    default.units = "in", just = c(0, 0),
                    gp = gpar(col = "blue", fill = rgb(0,0,1,0.1), lty=lty),
                    name = name)
          cnt = cnt + 1
        }
        grid.lines(y = matchInfo$y[index], default.units = "in",
                   gp = gpar(col = "blue"),
                   name = paste0("y.", nameFour(matchInfo$y[index],r), ".alignment"))
      }
    }
  }
  if (length(item[matches])>0)
    cat("\nAligned!!\n")
  for (i in seq_along(item[matches]))
    cat(item[matches][i], "\n")
  item[matches]
}
