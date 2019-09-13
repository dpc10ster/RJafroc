library(RJafroc)
rm(list = ls())
fileName <- system.file("extdata", "FrocData.xlsx",
package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
rm(fileName)
#resDBMH<- StSignificanceTesting(ds, FOM = "HrAuc")
#resOR <- StSignificanceTesting(ds, FOM = "HrAuc", method = "ORH")


pseudoValues <- ret$pseudoValues

msPseudovalues <- pseudoValueMeanSquares(pseudoValues)
# msT <- msPseudovalues$msT
msR <- msPseudovalues$msR
msC <- msPseudovalues$msC
msTR <- msPseudovalues$msTR
msTC <- msPseudovalues$msTC
msRC <- msPseudovalues$msRC
msTRC <- msPseudovalues$msTRC

varR <- (msR - msTR - msRC + msTRC)/(I * K)
varC <- (msC - msTC - msRC + msTRC)/(I * J)
varTR <- (msTR - msTRC)/K
varTC <- (msTC - msTRC)/J
varRC <- (msRC - msTRC)/I
varErr <- msTRC
varComp <- c(varR, varC, varTR, varTC, varRC, varErr)
varCompName <- c("Var(R)", "Var(C)", "Var(T*R)", "Var(T*C)", "Var(R*C)", "Var(Error)")
varComp <- data.frame(varComp, row.names = varCompName)
# 
# # if (VarCompFlag){
# #   return (varComp)
# # }
# 
# varCompDBM <- list(
#   varR = varR,
#   varC = varC,
#   varTR = varTR,
#   varTC = varTC,
#   varRC = varRC,
#   varErr = varErr
# )
# 
varCompDBM <- data.frame(
  varR = varR,
  varC = varC,
  varTR = varTR,
  varTC = varTC,
  varRC = varRC,
  varErr = varErr
)

varCompOR <- UtilDBM2ORVarComp (K, varCompDBM)
varCompOR <- data.frame(
  varR = varCompOR$varR,
  varTR = varCompOR$varTR,
  Cov1 = varCompOR$Cov1,
  Cov2 = varCompOR$Cov2,
  Cov3 = varCompOR$Cov3,
  varErr = varCompOR$varErr
)


