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
  print(gridGraphics::grid.echo(g))
}

g2plot.recordedplot <- function(g) {
  replayPlot(g)
  gridGraphics::grid.echo()
}
