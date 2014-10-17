#' Shows the various plotting symbols
#' 
#' @details This was found and copied from here: http://rgraphics.limnology.wisc.edu/pch.php
#' @export
pchShow <-  function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
                     cex = 3, ## good for both .Device=="postscript" and "x11"
                     col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
                     main = paste("plot symbols :  points (...  pch = *, cex =",
                                  cex,")")) {
  # From R help for points graphics - shows point values for plots
  nex <- length(extras)
  np  <- 26 + nex
  ipch <- 0:(np-1)
  k <- floor(sqrt(np))
  dd <- c(-1,1)/2
  rx <- dd + range(ix <- ipch %/% k)
  ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
  pch <- as.list(ipch) # list with integers & strings
  if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
  plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
       main = main)
  abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
  for(i in 1:np) {
    pc <- pch[[i]]
    ## 'col' symbols with a 'bg'-colored interior (where available) :
    points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
    if(cextext > 0)
      text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
  }
}


#' Create adjacent rectangular polygons
#' @description Generic function to create adjacent rectangular polygons from vectors of coordinates passed to 
#' it
#' @param x Vector of x coordinates, arranged in ascending order
#' @param y Vector of y coordinates, arrange in acending order
#' @param fillcol A 1 to n - 1 length vector of colors for filling. n = length of x or y (the longest)
#' @param linecol A 1 to n - 1 length vector of polygon border colors. n = length of x or y (the longest)
#' @return A polygon "chain": segmented polygons with interior vertical x or horizontal y axis
#' @details This is modeled on the rect_coords function used by flex_legend, but the resulting polygon 
#' can be of arbitrary shape. It can be used to make either a horizontal (i.e. a sequence of polygons
#' separated on the X-axis, with the long dimension filling the Y-axis) or vertical chain of polygons (the
#' opposite). To make the former, provide a vector giving the X-coordinates separating each polygon, and a 2 
#' parameter vector specifying the lower and upper Y coordinates. For the opposite case, give the left and 
#' right X coordinates to X, and a vector of Y coordinates to Y. 
#' @examples
#' a <- c(-50, -25, 0, 25, 50)
#' b <- c(-1, 21)
#' plot(c(-100, 100), c(-1, 22), pch = "")
#' rect_shade(b, a)
#' 
#' plot(c(-100, 100), c(-1, 22), pch = "")
#' rect_shade(a, b)
#' 
#' cols <- sapply(c(0.8, 0.5, 0.2, 0.1), function(x) rgb(1, 0, 0, alpha = x))
#' plot(c(-1, 22), c(-100, 100), pch = "")
#' # rect_shade(b, a, fillcol = cols)
#' rect_shade(b, a, fillcol = cols, linecol = cols)
#' @export

rect_shade <- function(x, y, fillcol = "grey", linecol = "black", lwd = 1) {
  vlengths <- c(length(x), length(y))
  xydir <- which.max(vlengths)
  if(length(fillcol) != (vlengths[xydir] - 1)) fillcol <- rep(fillcol[1], vlengths[xydir] - 1)
  if(length(linecol) != (vlengths[xydir] - 1)) linecol <- rep(linecol, vlengths[xydir] - 1)
  rects <- lapply(1:(vlengths[xydir] - 1), function(j) {
    if(xydir == 1) {
      pols <- polygon(x = c(x[j], rep(x[j + 1], 2), x[j]), y = c(y[1], y[1], y[2], y[2]), col = fillcol[j], 
                      border = linecol[j], lwd = lwd)
    }
    if(xydir == 2) {
      pols <- polygon(x = c(x[1], x[1], x[2], x[2]), y = c(y[j], rep(y[j + 1], 2), y[j]), col = fillcol[j], 
                      border = linecol[j], lwd = lwd)
    }
  })
}

