# homeBoxPlot.R

homeBoxPlot <- function(x, ycoord, inhgt = 20, whiskhgt, bcol, bfill, whiskcol, n, pcex) {
 # Custom horizontal boxplot function, to be used with dummy plot call specify number of boxplots to be placed
 #  on the Y axis
 # Args: 
 #  x: vector containing, respectively, the 5, 25, 50, 75, 95th percentile values and the mean.
 #  ycoord: Y coordinate for plotting boxplot
 #  inhgt: height of the interquartile box, scaled to size of device and number of boxes to be plotted
 #  whiskhgt: height of the 95th percentile whiskers, scaled in the same way as inhgt
 #  bcol: Color of lines (median line, whiskers and interquartile outline) and mean point
 #  bfill: Fill color for interquartile box
 #  n: Number of boxplots to be placed on plot
 #  pcex: Size of point indicating the mean 
 
 insc <- par()$din[1] / n * inhgt / 100 / 2
 whisksc <- par()$din[1] / n * whiskhgt / 100
 arrows(x[1], ycoord, x[2], ycoord, angle = 90, code = 1, length = whisksc, col = bcol)
 arrows(x[4], ycoord, x[5], ycoord, angle = 90, code = 2, length = whisksc, col = bcol)
 rect(xleft = x[2], ybottom = ycoord - insc, xright = x[4], ytop = ycoord + insc, col = bfill, 
      border = bcol)
 lines(rep(x[3], 2), c(ycoord - insc, ycoord + insc), lwd = 3, col = bcol, lend = 2)
 points(x[6], ycoord, pch = 20, col = bcol, cex = pcex)
}

homeBoxPlotX <- function(xcoord, y, inhgt = 20, whiskhgt, bcol, bfill, whiskcol, n, pcex) {
  # Custom horizontal boxplot function, to be used with dummy plot call specify number of boxplots to be placed
  #  on the X axis
  # Args: 
  #  xcoord: X coordinate for plotting boxplot
  #  y: vector containing, respectively, the 5, 25, 50, 75, 95th percentile values and the mean.
  #  inhgt: width of the interquartile box, scaled to size of device and number of boxes to be plotted
  #  whiskhgt: width of the 95th percentile whiskers, scaled in the same way as inhgt
  #  bcol: Color of lines (median line, whiskers and interquartile outline) and mean point
  #  bfill: Fill color for interquartile box
  #  n: Number of boxplots to be placed on plot
  #  pcex: Size of point indicating the mean 
 
  insc <- par()$din[2] / n * inhgt / 100 / 2
  whisksc <- par()$din[2] / n * whiskhgt / 100
  arrows(xcoord, y[1], xcoord, y[2], angle = 90, code = 1, length = whisksc, col = whiskcol, lwd = 3)
  arrows(xcoord, y[4], xcoord, y[5], angle = 90, code = 2, length = whisksc, col = whiskcol, lwd = 3)
  rect(xleft = xcoord - insc, ybottom = y[2], xright = xcoord + insc, ytop = y[4], col = bfill, 
       border = bcol, lwd = 3)
  lines(c(xcoord - insc, xcoord + insc), rep(y[3], 2), lwd = 3, col = bcol, lend = 2)
  points(xcoord, y[6], pch = 20, col = bcol, cex = pcex)
}