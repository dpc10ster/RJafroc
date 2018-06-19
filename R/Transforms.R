ForwardZetas <- function (zetas) {
  zetasFwd <- zetas
  zetasFwd[1] = zetas[1];
  if (length(zetas) > 1){
    zetasFwd[2:length(zetasFwd)] <- log(zetasFwd[2:length(zetasFwd)] - 
                                          zetasFwd[1:(length(zetasFwd) - 1)])
  }
  return (zetasFwd)
}


InverseZetas <- function(zetasFwd){
  zetas <- zetasFwd
  if (length(zetasFwd) > 1) {
    for (i in 2:length(zetasFwd)) zetas[i] <- exp(zetasFwd[i]) + zetas[i - 1]
  }
  return (zetas)
}
