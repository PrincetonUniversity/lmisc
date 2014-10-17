#' Find graphics device coordinates for rectangles
#' @description Finds graphics device coordinates for rectangles that can be used to construct image legend
#' @param minWorS The minimum X (horizontal legend) or Y (vertical) user coordinate (0 - 1) for the legend
#' @param maxEorN The maximum X (horizontal legend) or Y (vertical) user coordinate (0 - 1) for the legend 
#' @param ncuts Number of intervals for legend
#' @param EWorNS "EW" for horizontal legend (default) or "NS" for vertical
#' @param constEWorNS The coordinate that will remain constant (X coord for vertical legend, Y for horiz)
#' @param resEWorNS The width in user coordinate for the legend
#' @return List of coordinates for the four corners of each segment of legend, in graphical device coords
#' @details This function depends on the TeachingDemos library, and still uses the deprecated cnvrt.coords().
#' @export
rect_coords <- function(minWorS, maxEorN, ncuts, EWorNS = "EW", constEWorNS, resEWorNS) {
  space <- diff(c(minWorS, maxEorN)) / ncuts  # Resolution in map coordinates for legend boxes 
  crds <- seq(minWorS, maxEorN, space)  # map coordinates for legend boxes
  vcrd <- rbind(crds[-length(crds)], crds[-1])  # Segment coordinates (legend intervals)
  
  if(EWorNS == "NS") {
    rects <- lapply(1:ncuts, function(x) {
      rbind("ll" = c("x" = constEWorNS, "y" = vcrd[1, x]),
            "lr" = c("x" = constEWorNS + resEWorNS, "y" = vcrd[1, x]), 
            "ul" = c("x" = constEWorNS, "y" = vcrd[2, x]),
            "ur" = c("x" = constEWorNS + resEWorNS, "y" = vcrd[2, x]))    
    })
  } else if(EWorNS == "EW") {
    rects <- lapply(1:ncuts, function(x) {
      rbind("ll" = c("x" = vcrd[1, x], "y" = constEWorNS),
            "lr" = c("x" = vcrd[2, x], "y" = constEWorNS), 
            "ul" = c("x" = vcrd[1, x], "y" = constEWorNS + resEWorNS),
            "ur" = c("x" = vcrd[2, x], "y" = constEWorNS + resEWorNS))    
    })
  }  
  rtdev <- lapply(1:ncuts, function(x) {
    #do.call("cbind", TeachingDemos::cnvrt.coords(rects[[x]][, 1], rects[[x]][, 2], 'tdev')$usr)
    cbind(grconvertX(rects[[x]][, 1], from = "ndc", to = "user"), 
          grconvertY(rects[[x]][, 2], from = "ndc", to = "user"))
  })
  return(rtdev)
}

#' Create a legend where you want it
#' @description Creates a vertical or horizontal legend in a location of your choosing
#' @param ncuts Number of intervals for legend
#' @param legend.text Label for the legend (e.g. units)
#' @param legend.vals Vector of labels for the legend
#' @param legend.pos A 1 or 2 element vector that specifies where along vector legend.text should be (details)
#' @param longdims Vector specifying the user coordinates for the legend's long axis (e.g. c(0.2, 0.8))
#' @param shortdims 1) anchor user coordinate for the short axis; 2) width of short axis (e.g. c(0.05, 0.02))
#' @param colvec Vector of colors to fill each polygon in vector, equal in length to ncuts
#' @param leg.adj Tweak position(s) of legend.vals and/or legend.text (see details) 
#' @param cex.val Adjusts the text size of legend.vals and/or legend.text (see details)
#' @param srt.val Rotation of legend.vals and/or legend.text (see details)
#' @param horiz TRUE (default) or FALSE for horizontal or vertical legend
#' @param textside Specify "bottom" (default) or "top" for side of legend.vals on legend
#' @param textcol Vector of colors for legend.vals and legend.text
#' @param bordercol Outline color for polygon borders in legend
#' @return A polygon based legend
#' @details Legend.pos uses a two parameter vector, with the second value specifying the polygon number in the 
#' legend next to/above/below which the legend.text will appear, and the first which of the four corners of  
#' the polygon it will be closest to. Default values are chosen if just one (which then specifies polygon 
#' number)vor no values are provided. leg.adj is passed as either a list of two two element vectors, or a 
#' single two element vector. If a list, the first element specifies adjustments for legend.values, and the 
#' second element adjustments for the legend.text. If just a vector, then these values are used for both. e.g. 
#' list(c(0, 0), c(-1, -0.5)); c(0, -0.5). 
#' @name flex_legend
#' @rdname flex_legend
#' @examples 
#' library(raster)
#' r1 <- raster(nrow = 20, ncol = 20)
#' r1[] <- sample(0:75, ncell(r1), replace = TRUE)
#' brks <- round(quantile(r1, seq(0, 1, 0.1)))
#' cols <- colorRampPalette(c("red", "antiquewhite", "blue"))
#' par(mar = c(5, 4, 0, 0))
#' plot(r1, axes = FALSE, box = FALSE, legend = FALSE, breaks = brks, col = cols(length(brks) - 1))
#' flex_legend(ncuts = length(brks) - 1, legend.text = "vals", leg.adj = list(c(0.5, 1), c(0, -0.5)), 
#'             legend.vals = brks, legend.pos = c(4, 5), longdims = c(0.2, 0.8), shortdims = c(0.08, 0.03), 
#'             colvec = cols(length(brks) - 1))
#' dev.off()
#' 
#' par(mar = c(0, 4, 0, 4))
#' plot(r1, axes = FALSE, box = FALSE, legend = FALSE, breaks = brks, col = cols(length(brks) - 1))
#' flex_legend(ncuts = length(brks) - 1, legend.text = "vals", textside = "right", 
#'             legend.pos = c(3, 10), leg.adj = list(c(0.2, 0.5), c(0, -1)), 
#'             horiz = FALSE, legend.vals = brks, longdims = c(0.2, 0.8), 
#'             shortdims = c(0.85, 0.02), colvec = cols(length(brks) - 1))
#'             dev.off()
#' @export
flex_legend <- function(ncuts, legend.text, legend.vals, legend.pos, longdims, shortdims, colvec, 
                       leg.adj = c(0, 0), cex.val = 1, srt.val = 0, horiz = TRUE, textside = "bottom", 
                       textcol = "black", bordercol = "black") {
  
  lbump <- 0
  if(missing(legend.pos)) {
    lp <- NULL
    lbump <- 0.06
    print("Missing legend position")
  } else {
    lp <- legend.pos
  }
  if(is.list(leg.adj)) {
    ladj1 <- leg.adj[[1]]
    ladj2 <- leg.adj[[2]]
  } else {
    ladj1 <- leg.adj
    ladj2 <- leg.adj
  }
  if(length(cex.val) > 1) {
    cex1 <- cex.val[1]
    cex2 <- cex.val[2]
  } else {
    cex1 <- cex.val
    cex2 <- cex.val
  }
  if(length(srt.val) > 1) {
    srt1 <- srt.val[1]
    srt2 <- srt.val[2]
  } else {
    srt1 <- srt.val
    srt2 <- srt.val
  }
  ncuts <- ncuts
  par(xpd = NA)
  if(horiz == TRUE) {
    direction <- "EW"
    if(is.null(lp)) lp <- c(2, ceiling(ncuts / 2)) 
    if(length(lp) < 2) lp <- c(2, legend.pos)
    if(textside == "bottom") tp <- side <- c(1, 2)
    if(textside == "top") tp <- c(3, 4)
    if(textside == "left" | textside == "right") stop("Horizontal legends must have labels on top or bottom")
  } else if(horiz == FALSE) {
    direction <- "NS"
    if(is.null(lp)) lp <- c(3, ncuts)
    if(length(lp) < 2) lp <- c(3, legend.pos)
    if(textside == "left") tp <- c(1, 3)
    if(textside == "right") tp <- c(2, 4)
    if(textside == "top" | textside == "bottom") stop("Vertical legends must have labels on left or right")
  }
  crds <- rect_coords(minWorS = longdims[1], maxEorN = longdims[2], ncuts = ncuts, EWorNS = direction, 
                      constEWorNS = shortdims[1], resEWorNS = shortdims[2])
  leg <- sapply(1:length(crds), function(j) {
      rect(crds[[j]][1, 1], crds[[j]][1, 2], crds[[j]][4, 1], crds[[j]][4, 2], col = colvec[j], 
           border = bordercol)
  })
  if(horiz == TRUE) {  
    tcrds <- rbind(crds[[1]][tp[1], ], do.call("rbind", lapply(1:ncuts, function(j) crds[[j]][tp[2], ])))
    if(textside == "bottom") tcrds[, 2] <- tcrds[, 2] - abs(tcrds[, 2] * 0.02)
    if(textside == "top") tcrds[, 2] <- tcrds[, 2] + abs(tcrds[, 2] * 0.02)
  }
  if(horiz == FALSE) {
    tcrds <- rbind(crds[[1]][tp[1], ],  do.call("rbind", lapply(1:ncuts, function(j) crds[[j]][tp[2], ])))
    if(textside == "left") tcrds[, 1] <- tcrds[, 1] - abs(tcrds[, 1] * 0.02)
    if(textside == "right") tcrds[, 1] <- tcrds[, 1] + abs(tcrds[, 1] * 0.02)
  }
  text(x = tcrds[, 1], y = tcrds[, 2], labels = legend.vals, adj = ladj1, cex = cex1, srt = srt1, 
       col = textcol)
  lcrds <- crds[[lp[2]]][lp[1], ]
  text(x = lcrds[1], y = lcrds[2] + abs(lcrds[2] * lbump), legend.text, cex = cex2, adj = ladj2, 
       srt = srt2, col = textcol)
}