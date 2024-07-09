calculateR0 <- function(beta = 0, gamma = 0, sigma = 0, delta = 0, xi = 0){
  #SI-Type
  if (sigma == 0){
    #if the model is SIR/SIRS/SIRD type
    if (delta == 0){ R0 <- beta/gamma} else {R0 <- beta/(gamma + delta)}
  }
  return(R0)
}
#test <- calculateR0(beta =  0.1726, gamma = 0.09556, sigma = 0, delta = 0, xi = 0)
#print(test)