fileName1 <- system.file(
  "extdata", "toyFiles/roc/roc1.xlsx", package = "RJafroc", mustWork = TRUE)
fileName2 <- system.file(
  "extdata", "toyFiles/roc/roc2.xlsx", package = "RJafroc", mustWork = TRUE)

ds1 <- DfReadDataFile(fileName1, newExcelFileFormat = T)
ds2 <- DfReadDataFile(fileName2, newExcelFileFormat = T)

I <- 2
J_1 <- length(ds1$ratings$NL[1,,1,1])
J_2 <- length(ds2$ratings$NL[1,,1,1])

varCom1 <- UtilVarComponentsOR(ds1, FOM = "Wilcoxon")
varCom2 <- UtilVarComponentsOR(ds2, FOM = "Wilcoxon")

st1 <- StSignificanceTesting(ds1, FOM = "Wilcoxon")
st2 <- StSignificanceTesting(ds2, FOM = "Wilcoxon")

Ac_1 <- array(dim = c(J_1, 2))
Ac_2 <- array(dim = c(J_2, 2))

for (i in 1:2) {
  for (j in 1:J_1) {
    Ac_1[j,i] <- st1$FOMs$foms[i,j]
  }
}

for (i in 1:2) {
  for (j in 1:J_2) {
    Ac_2[j,i] <- st2$FOMs$foms[i,j]
  }
}

Ac <- rbind(Ac_1, Ac_2)

# J <- J_1 + J_2
# varCom <- array(0, dim = c(I, I, J, J))
# 
# for (i in 1:I) {
#   for (j1 in 1:J) {
#     for (j2 in 1:J) varCom[i,i,j1,j2] <- varCom1$VarCom
#   }
# }

