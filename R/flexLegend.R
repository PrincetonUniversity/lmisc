# flexLegend.R

library(TeachingDemos)
flexLegend <- function(ncuts, legend.text, legend.vals, legend.pos, longdims, shortdims, colvec, 
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
  } else if(horiz == FALSE) {
    direction <- "NS"
    if(is.null(lp)) lp <- c(3, ncuts)
    if(length(lp) < 2) lp <- c(3, legend.pos)
    if(textside == "left") tp <- c(1, 3)
    if(textside == "right") tp <- c(2, 4)
  }
  crds <- rectCoordsLeg(minWorS = longdims[1], maxEorN = longdims[2], ncuts = ncuts, EWorNS = direction, 
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

