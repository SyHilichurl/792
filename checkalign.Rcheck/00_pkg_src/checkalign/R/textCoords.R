#' @title Convert TextGrobs
#' @description Separate text elements
#' @param tg one textgrob
#'
#' @return updated textgrob with separate text elements
#' @export
#'
#' @examples
textCoords <- function(tg) {
  ntext <- max(length(tg$x), length(tg$y))
  label <- rep(tg$label, length.out=ntext)
  x <- rep(tg$x, length.out=ntext)
  y <- rep(tg$y, length.out=ntext)
  if (is.null(tg$hjust)) {
    hjust <- vector("list", ntext)
  } else {
    hjust <- rep(tg$hjust, length.out=ntext)
  }
  if (is.null(tg$vjust)) {
    vjust <- vector("list", ntext)
  } else {
    vjust <- rep(tg$vjust, length.out=ntext)
  }
  rot <- rep(tg$rot, length.out=ntext)
  gp <- tg$gp
  if (length(gp)) {
    ngp <- max(sapply(gp, length))
    gps <- lapply(gp, function(x) rep(x, length.out=ngp))
    gp <- lapply(1:ngp,
                 function(i) do.call(gpar,
                                     lapply(gps, function(x) x[i])))
  } else {
    gp <- lapply(1:ntext, function(i) gpar())
  }
  coords <- mapply(function(l, x, y, hjust, vjust, rot, gp,
                            just, check.overlap, name, vp) {
    # closed required
    grobCoords(textGrob(l, x, y, just,
                        hjust, vjust, rot,
                        check.overlap, name, gp, vp),closed=TRUE)
  },
  label, x, y, hjust, vjust, rot, gp,
  MoreArgs=list(tg$just,
                tg$check.overlap,
                paste0(tg$name, ".", 1:ntext),
                tg$vp),
  SIMPLIFY=FALSE)
  gridCoords <- lapply(coords, function(x) x[[1]])
  names(gridCoords) <- 1:ntext
  gridGrobCoords(gridCoords, name=tg$name)
}
