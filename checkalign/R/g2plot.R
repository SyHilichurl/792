g2plot <- function(g) {
  UseMethod("g2plot")
}

g2plot.ggplot <- function(g) {
  print(g)
  grid.force()
}

g2plot.trellis <- function(g) {
  print(g)
}

g2plot.function <- function(g) {
  grid.newpage()
  tryCatch(
    {gridGraphics::grid.echo(g)},
    warning = function(w) {
      if (grepl("No graphics to replay", w)) {
        dev.off()
        unlink("plot0.png")
        stop("Make sure the function is a plot\n.")
      }
    })
}


g2plot.recordedplot <- function(g) {
  dev.control("enable")
  replayPlot(g)
  gridGraphics::grid.echo()
}

