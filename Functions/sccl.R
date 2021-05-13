#function to plot a time series of conductivity with chloride points overlain


#a much better method than the one below:

sc_cl <- function(logger, lab, no) {
  ggplot() +
    geom_line(PBMS_cond_data, mapping = aes(date, runningmean)) + 
    geom_point(labPBMS, mapping = aes(date, chloride_mgL*6), color = "#F24D29") +
    scale_y_continuous(
      name = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C, 
      sec.axis = sec_axis(~./6, name = "Chloride Concentration"~(mg~L^-1))
    ) +
    theme(axis.title.y.right = element_text(color = "#F24D29")) +
    labs(x= "") +
    L_theme()
  
}




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
  
  plot(logger$date, logger$runningmean,
       col="#27223C",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE)
  
  axis(4, col = "#27223C", col.axis = "#27223C")
  
  mtext("Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C, side = 4, line = 3, col = "#27223C")
}
