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
#' checkAlignment(g, show = "both", align="b",
#' include=".", exclude=NULL, separateText = TRUE, rounding=4)
#' @param g
#' A graphic value (e.g. a ggplot)
#' @param show
#' A character value indicating which hints to be shown.
#' Possible values are "both", "unaligned", and "aligned".
#' @param facet
#' A character value indicating whether the plot showing aligned elements to be shown in facets.
#' Default value is "on". If "on", all the subplots (no more than 25) will be shown in facets.
#' If "off", all the subplots will be shown in one.
#' If "page", all the subplots will be shown along with prompting to the user.
#' @param align
#' A numeric value indicating which hints of aligned elements to be shown.
#' "v" means vertical alignments, "h" means horizontal alignments and default value "b"
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
#' @return A table to show the number of each aligned element's pairs.
#' @export
#'
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
checkAlignment <- function(g, show = "both", facet = "on",
                           align = "b", include=".", exclude=NULL,
                           separateText = TRUE, rounding=4) {
  show <- match.arg(show, c("both", "unaligned", "aligned"))
  facet <- match.arg(facet, c("on", "off", "page"))
  align <- match.arg(align, c("b", "v", "h"))
  UseMethod("checkAlignment")
}

#' @export
checkAlignment.ggplot <- function(g, show = "both", facet = "on",
                                  align="b", include=".", exclude=NULL,
                                  separateText = TRUE, rounding=4) {
  png("plot0.png", width=400, height=400)
  g2plot(g)
  res <- checkAndDraw(g, show, facet, align, include, exclude, separateText, rounding)
  invisible(res)
}

#' @export
checkAlignment.trellis <- function(g, show = "both", facet = "on",
                                   align="b", include=".", exclude=NULL,
                                   separateText = TRUE, rounding=4) {
  png("plot0.png", width=400, height=400)
  g2plot(g)
  res <- checkAndDraw(g, show, facet, align, include, exclude, separateText, rounding)
  invisible(res)
}

#' @export
checkAlignment.function <- function(g, show = "both", facet = "on",
                                    align="b", include=".", exclude=NULL,
                                    separateText = TRUE, rounding=4) {
  png("plot0.png", width=400, height=400)
  g2plot(g)
  res <- checkAndDraw(g, show, facet, align, include, exclude, separateText, rounding)
  invisible(res)
}

#' @export
checkAlignment.recordedplot <- function(g, show = "both", facet = "on",
                                        align="b", include=".", exclude=NULL,
                                        separateText = TRUE, rounding=4) {
  png("plot0.png", width=400, height=400)
  g2plot(g)
  res <- checkAndDraw(g, show, facet, align, include, exclude, separateText, rounding)
  invisible(res)
}
