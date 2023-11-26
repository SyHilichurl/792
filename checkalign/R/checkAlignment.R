#' checkAlignment
#'
#' @description
#' Check if the arrangement of elements follows the design principle of alignment,
#' that is all the elements are aligned with at least one another element of the
#' plot horizontally or vertically.
#' Mark the isolated elements in red, and aligned elements in blue.
#' Return a list of aligned elements' names.
#'
#' @usage
#' checkAlignment(g, show = "both", align=.5,
#' include=".", exclude=NULL, separateText = TRUE, rounding=4)
#' @param x
#' A graphic value (e.g. a ggplot)
#' @param show
#' A character value indicating which hints to be shown.
#' Possible values are "both", "unaligned", and "aligned".
#' @param align
#' A numeric value indicating which hints of aligned elements to be shown.
#' 0 means vertical alignments, 1 means horizontal alignments and default value 0.5
#' means both.
#' @param include
#' A character value or vector indicating which kind of elements will be checked.
#' Default value is "." (e.g. "text", "axis", "lab")
#' @param exclude
#' A character value or vector indicating which kind of elements will not be checked.
#' Default value is NULL. (e.g. "text", "axis", "lab")
#' @param separateText
#' A logical value indicating if the elements in a textgrob will be separated.
#' If "TRUE", all the element in a textgrob will be regarded as different elements.
#' If "FALSE", all the element in a textgrob will be regarded as a unity.
#' @param rounding
#' A numeric value indicating the decimal point precision used in checking.
#'
#' @return A table to show the aligned elements.
#' @export
#'
#' @importFrom checkAndDraw
#' @seealso  \code{\link[grid]{grid.ls}}
#' @examples \dontrun{
#'   if (require(ggplot2)) {
#'     g <- ggplot(mtcars) + geom_point(aes(disp, mpg, color=factor(vs))) +
#'          labs(title = "test") + facet_wrap(~gear)
#'     checkAlignment(g, include="text", exclude=c("tag", "points"), rounding=4)
#'   } else {
#'     warning("The example requires 'ggplot2' which is not installed.")
#'   }
#' }
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
