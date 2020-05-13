#function to plot a time series of conductivity with chloride points overlain
#I don't know  if this will be useful
sccl <- function(logger, lab) {
  
  par(mar = c(5,5,5,5)) 
  
  plot(lab$datetime_collected, lab$chloride_mgL,
       col = "#D1362F", pch = 19,
       ylab = "",
       xlab = "",
       yaxt = "n")
  
  axis(2, col = "#D1362F", col.axis = "#D1362F")
  
  mtext("Chloride Concentration"~(mg~L^-1), side = 2, line = 3, col = "#D1362F")
  
  par(new = TRUE)
  
  plot(logger$date, logger$sp.cond,
       col="#27223C",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE)
  
  axis(4, col = "#27223C", col.axis = "#27223C")
  
  mtext("Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~, side = 4, line = 3, col = "#27223C")
}