# rectCoordsLeg.R

rectCoordsLeg <- function(minWorS, maxEorN, ncuts, EWorNS = "EW", constEWorNS, resEWorNS) {
  # Finds graphics device coordinates for rectangles that can be used to construct image legend
  # Args: 
  #   minWorS: The minimum X (horizontal legend) or Y (vertical) user coordinate (0 - 1) for the legend
  #   maxEorN: The maximum X (horizontal legend) or Y (vertical) user coordinate (0 - 1) for the legend 
  #   ncuts: Number of intervals for legend
  #   EWorNS: "EW" for horizontal legend (default) or "NS" for vertical
  #   constEWnorS: The coordinate that will remain constant (e.g. the x coord for vertical legend, Y for horiz)
  #   resEWorNS: The width in user coordinate for the legend
  # Returns: 
  #   List of coordinate pairs specifying the four corners of each segment of legend, in graphical device coords
  # Notes: Requires TeachingDemos
  
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
    do.call("cbind", cnvrt.coords(rects[[x]][, 1], rects[[x]][, 2], 'tdev')$usr)  # Convert to graphic dev  
  })
  return(rtdev)
}













