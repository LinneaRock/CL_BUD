#function to convert actual conductivity to specific conductivity 
SC <- function(ActualCond, temp) {
  ActualCond / (1 - (25 - temp) * 0.021)
}
