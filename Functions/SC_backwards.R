#calculate actual conductivity from specific conductivity 
SC_backwards <- function(Spec_cond, temp) {
  Spec_cond * (1 - (25 - temp) * 0.021)
}