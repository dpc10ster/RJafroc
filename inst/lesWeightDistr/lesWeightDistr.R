## Following example is for mu = 2, lambda = 1, nu = 0.6, in one treatment and   
## mu = 3, lambda = 1.5, nu = 0.8, in the other treatment. 20% of the diseased 
## cases have a single lesion, 40% have two lesions, 10% have 3 lesions, 
## and 30% have 4 lesions.  
lesDistr <- rbind(c(1, 0.2), c(2, 0.4), c(3, 0.1), c(4, 0.3))

## On cases with one lesion the weights are 1, on cases with 2 lesions the weights
## are 0.4 and 0.6, on cases with three lesions the weights are 0.2, 0.3 and 0.5, and
## on cases with 4 lesions the weights are 0.3, 0.4, 0.2 and 0.1: 
lesWghtDistr <- rbind(c(1.0, -Inf, -Inf, -Inf), 
                      c(0.4,  0.6, -Inf, -Inf), 
                      c(0.2,  0.3,  0.5, -Inf), 
                      c(0.3,  0.4, 0.2,  0.1))
ret <- PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                       lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, 
                                       legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))