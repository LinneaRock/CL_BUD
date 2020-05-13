#Function to save plot
#X = "folder name/"
#Y = "name of plot"

splot <- function(X, Y) {
  
ggsave(filename = paste("Plots/", X, Y, ".png", sep = ""), width = 6, height = 4, units = 'in')
  
}