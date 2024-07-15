calculateR0 <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                        trueMassAction = FALSE){
  
  N <- population^(abs(as.numeric(trueMassAction) - 1))

  #SI-Type
  if (sigma == 0){
    #SIRS Model
    if (xi != 0) {
      R0 <- beta / (gamma + xi)
    } else if (delta != 0) { #SIRD Model
      R0 <- beta / (gamma + delta)
    } else { #SIR Model
      R0 <- beta / gamma 
    }
  } else { #SEI- Type
    if (xi != 0) { #SEIRS Model
      R0 <- beta / (sigma + xi)
    } else if (delta != 0) { #SEIRD Model
      R0 <- beta / (sigma + delta)
    } else {
      R0 <- beta / sigma
    }
  }

  R0 <- R0 * N
  return(R0)
}
#test <- calculateR0(population = 999, beta =  0.1726, gamma = 0, sigma = 0.09556, delta = 0.05, xi = 0,
#                    trueMassAction = FALSE)
#print(test)