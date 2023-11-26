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
  cat(res)
}

checkalign3 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg))
  } else {
    cat("The example requires 'ggplot2' which is not installed.")
  }
  checkAlignment(g2, rounding=3, show = "aligned", align=0)
}

checkalign4 <- funtion() {
  a <- xyplot(mpg ~ disp | vs, data = mtcars, type = "p", main = "Scatterplot Example")
  checkAlignment(a, exclude="background",
                 include = c("lab", "textl"), rounding=2)
}

checkalign5 <- function() {
  x <- function() plot(mtcars$mpg, mtcars$vs)
  checkAlignment(x, include="axis")
}

checkalign6 <- function() {
  x1 <- function() print("yes")
  checkAlignment(x1, include="axis")
}

checkalign7 <- funtion() {
  plot(mtcars$mpg, mtcars$disp)
  y <- recordPlot()
  replayPlot(y)
  checkAlignment(y, include="axis")
}



