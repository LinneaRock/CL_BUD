#Function to save plot
#X = "folder name/"
#Y = "name of plot"

splot <- function(folder, plotname) {
  
ggsave(filename = paste("Plots/", folder, plotname, ".png", sep = ""), width = 6, height = 4, units = 'in')
  
}