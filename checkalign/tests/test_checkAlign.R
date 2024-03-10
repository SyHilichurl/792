library(checkalign)

checkalign1 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, rounding=4, show = "unaligned")
  res
}

checkalign2 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, include="text", exclude=c("tag", "points"), rounding=4)
  res
}

checkalign3 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, showInOne=TRUE, include="text",
                        exclude=c("tag", "points"), rounding=4)
  res
}

checkalign4 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
      labs(title = "test") + facet_wrap(~gear)
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, showInOne="hi", include="text",
                        exclude=c("tag", "points"), rounding=4)
  # res <- checkAlignment(g, showInOne="hi", include="text", show = "unaligned",
  #                       exclude=c("tag", "points"), rounding=4)
  # res <- checkAlignment(g, showInOne="hi", include="text", show = "aligned",
  #                       exclude=c("tag", "points"), rounding=4)
  res
}



checkalign5 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg))
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, rounding=3, show = "aligned", align="v")
  res
}

checkalign6 <- function() {
  if (require(ggplot2)) {
    g <- ggplot(mtcars) + geom_point(aes(disp, mpg))
  } else {
    warning("The example requires 'ggplot2' which is not installed.")
  }
  res <- checkAlignment(g, rounding=3, show = "aligned", align="b")
  res
}

checkalign7 <- function() {
  if (require(lattice)) {
    g <- xyplot(mpg ~ disp | vs, data = mtcars, type = "p", main = "Scatterplot Example")
  } else {
    warning("The example requires 'lattice' which is not installed.")
  }
  res <- checkAlignment(g, exclude="background",
                 include = c("lab", "textl"), rounding=2)
  res
}

checkalign8 <- function() {
  x <- function() plot(mtcars$mpg, mtcars$vs)
  res <- checkAlignment(x, include="axis")
  res
}

checkalign9 <- function() {
  x1 <- function() print("yes")
  res <- checkAlignment(x1, include="axis")
  res
}

checkalign10 <- function() {
  plot(mtcars$mpg, mtcars$disp)
  y <- recordPlot()
  replayPlot(y)
  res <- checkAlignment(y, include="axis")
  res
}



tryCatch({
  res <- checkalign2()
  if (any(grepl("xlab", res[[1]])) && any(grepl("ylab", res[[1]])) && length(res[[1]]) == 2) {
    message("Correct Result for Test 2\n")
  } else {
    warning("Wrong Result for Test 2\n")
  }
}, error = function(e) {
  warning("Test 2 Not Run\n")
})



tryCatch({
  res <- checkalign5()
  if (any(grepl("xlab", res[[1]])) && any(grepl("ylab", res[[1]])) && length(res[[1]]) == 2) {
    message("Correct Result for Test 5\n")
  } else {
    warning("Wrong Result for Test 5\n")
  }
}, error = function(e) {
  warning("Test 5 Not Run\n")
})

tryCatch({
  res <- checkalign9()
  if (any(grepl("xlab", res[[1]])) && any(grepl("ylab", res[[1]])) && length(res[[1]]) == 2) {
    message("Correct Result for Test 9\n")
  } else {
    warning("Wrong Result for Test 9\n")
  }
}, error = function(e) {
  warning("Test 9 Not Run\n")
})

