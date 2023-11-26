#' Title
#'
#' @param x graphic
#' @param show both, unaligned, aligned
#' @param include
#' @param exclude
#' @param separateText define if the elements textgrob need to be separated
#' @param rounding
#'
#' @return draw graphics with alignment hint and return a list of aligned grobs
#' @export
#'
#' @importFrom checkAndDraw
#'
#' @examples
checkAlignment <- function(x, show = "both", align=.5,
                           include=".", exclude=NULL,
                           separateText = TRUE, rounding) {
  UseMethod("checkAlignment")
}

#' @export
checkAlignment.ggplot <- function(x, show = "both", align=.5,
                                  include=".", exclude=NULL,
                                  separateText = TRUE, rounding=4) {
  print(x)
  grid.force()
  checkAndDraw(show, align, include, exclude, separateText, rounding)
}

#' @export
checkAlignment.trellis <- function(x, show = "both", align=.5,
                                   include=".", exclude=NULL,
                                   separateText = TRUE, rounding=4) {
  print(x)
  checkAndDraw(show, align, include, exclude, separateText, rounding)
}

#' @export
checkAlignment.function <- function(x, show = "both", align=.5,
                                    include=".", exclude=NULL,
                                    separateText = TRUE, rounding=4) {
  grid.newpage()
  print(gridGraphics::grid.echo(x))
  checkAndDraw(show, align, include, exclude, separateText, rounding)
}

#' @export
checkAlignment.recordedplot <- function(x, show = "both", align=.5,
                                        include=".", exclude=NULL,
                                        separateText = TRUE, rounding=4) {
  replayPlot(x)
  gridGraphics::grid.echo()
  checkAndDraw(show, align, include, exclude, separateText, rounding)
}
