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
# DBMVarComp <- list(
#   varR = varR,
#   varC = varC,
#   varTR = varTR,
#   varTC = varTC,
#   varRC = varRC,
#   varErr = varErr
# )
# 
DBMVarComp <- data.frame(
  varR = varR,
  varC = varC,
  varTR = varTR,
  varTC = varTC,
  varRC = varRC,
  varErr = varErr
)

varCompOR <- UtilDBM2ORVarComp (K, DBMVarComp)
varCompOR <- data.frame(
  varR = varCompOR$varR,
  varTR = varCompOR$varTR,
  Cov1 = varCompOR$Cov1,
  Cov2 = varCompOR$Cov2,
  Cov3 = varCompOR$Cov3,
  varErr = varCompOR$varErr
)

jkFOMArray[1,1,1:10]
#[1] 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9039394 0.9017677 0.9023232
kFOMArray[1,1,1:10]
#[1] 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9017677 0.9039394 0.9017677 0.9023232
#
#covariances[1,1,,]
# [,1]         [,2]         [,3]         [,4]
# [1,] 2.432013e-06 5.265564e-07 1.310391e-06 1.024621e-06
# [2,] 5.265564e-07 3.995157e-06 1.118645e-06 1.500788e-06
# [3,] 1.310391e-06 1.118645e-06 5.372022e-06 1.198496e-06
# [4,] 1.024621e-06 1.500788e-06 1.198496e-06 2.877510e-06
