library(checkalign)

checkalign1 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  checkAlignment(g, rounding=4, show = "unaligned")
}

checkalign2 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, include="text", exclude=c("tag", "points"), rounding=4)
  cat("\nAligned grobs number for each (name omit):", res)
}

checkalign3 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, showInOne=TRUE, include="text",
                        exclude=c("tag", "points"), rounding=4)
  cat("\nAligned grobs number for each (name omit):", res)
}

checkalign4 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg))
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, rounding=3, show = "aligned", align="v")
  cat("\nAligned grobs number for each (name omit):", res)
}

checkalign5 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg))
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, rounding=3, show = "aligned", align="b")
  cat("\nAligned grobs number for each (name omit):", res)
}

checkalign6 <- function() {
  if (require(lattice)) {
    g <- xyplot(mpg ~ disp | vs, data = mtcars, type = "p", main = "Scatterplot Example")
  } else {
    cat("The example requires 'lattice' which is not installed.")
  }
  checkAlignment(g, exclude="background",
                 include = c("lab", "textl"), rounding=2)
}

checkalign7 <- function() {
  x <- function() plot(mtcars$mpg, mtcars$vs)
  checkAlignment(x, include="axis")
}

checkalign8 <- function() {
  x1 <- function() print("yes")
  checkAlignment(x1, include="axis")
}

checkalign9 <- function() {
  plot(mtcars$mpg, mtcars$disp)
  y <- recordPlot()
  replayPlot(y)
  checkAlignment(y, include="axis")
}



