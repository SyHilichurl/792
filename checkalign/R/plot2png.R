plot2png <- function(g, w, h) {
  UseMethod("plot2png")
}

plot2png.ggplot <- function(g, w, h) {
  ggsave("plot0.png", plot = g, width = w, height = h, units = "in")
  img <- readPNG("plot0.png")
  unlink("plot0.png")
  img
}

plot2png.trellis <- function(g, w, h) {
  trellis.device(png, file = "plot0.png", width = w, height = h, units = "in", res=300)
  print(g)
  dev.off()
  img <- readPNG("plot0.png")
  unlink("plot0.png")
  img
}

plot2png.function <- function(g, w, h) {
  grid_plot <- gridGraphics::grid.echo(g)
  png("plot0.png", width = w, height = h, units = "in", res = 300)
  print(grid_plot)
  dev.off()
  img <- readPNG("plot0.png")
  unlink("plot0.png")
  img
}

