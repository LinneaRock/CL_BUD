#function to convert actual conductivity to specific conductivity 
SC <- function(AC, t) {
  AC / (1 - (25 - t) * .019)
}
