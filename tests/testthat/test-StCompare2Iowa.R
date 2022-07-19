# rm(list = ls())
# library(RJafroc)
# library(testthat)
library(stringr)

ExtractDataFrame <- function(lines, FindString, offSet, 
                             nRows, rowNames, CurrentLine, nCols) {
  
  if (FindString != "") 
  {while(lines[CurrentLine] != FindString) CurrentLine <- CurrentLine + 1}
  CurrentLine <- CurrentLine + offSet
  
  df <- NULL
  for (i in 1:nRows) {
    s <- lines[CurrentLine]
    if (rowNames[i] != "") s <- str_remove(s, rowNames[i])
    s <- str_remove(s, fixed("(")) 
    s <- str_remove(s, fixed(")")) 
    s <- str_remove(s, fixed("1 - 2")) 
    numCurrentLine <- as.numeric(Numextract(s))[nCols]
    dfCurrentLine <- t(data.frame(numCurrentLine,
                                  stringsAsFactors = FALSE))
    df <- rbind(df, dfCurrentLine)
    CurrentLine <- CurrentLine + 1
  }
  
  return(list(
    CurrentLine = CurrentLine,
    df = df
  ))
}

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:punct:]]?[[:digit:]]+\\.*[[:digit:]]*",string)))
}

contextStr <- "StSignificanceTesting: Compare to Iowa VanDyke dataset"
context(contextStr)
test_that(contextStr, {
  
fn <- paste0(test_path(), "/goodValues361/Iowa/VanDyke.txt")
# fn <- "inst/Iowa/VanDyke.txt"
lines <- readLines(fn)

DBM <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBM")
OR <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "OR")

CurrentLine <- 1
FindString <- "TREATMENT x READER AUC ESTIMATES"
offSet <- 6
nRows <- 5
rowNames <- as.character(seq(1:5))
nCols <- c(1,2)
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df1 <- ret$df

theirs <- df1;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- t(DBM$FOMs$foms);colnames(mine) <- NULL;rownames(mine) <- NULL
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

FindString <- " TREATMENT AUC MEANS (averaged across readers)"
offSet <- 2
nRows <- 2
rowNames <- as.character(seq(1:2))
nCols <- 1
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df2 <- ret$df

theirs <- df2;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- as.matrix(DBM$FOMs$trtMeans);colnames(mine) <- NULL;rownames(mine) <- NULL
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 2\n")

FindString <- " TREATMENT AUC MEAN DIFFERENCES"
offSet <- 2
nRows <- 1
rowNames <- "1 - 2"
nCols <- 1
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df3 <- ret$df

theirs <- df3;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- as.matrix(DBM$FOMs$trtMeanDiffs);colnames(mine) <- NULL;rownames(mine) <- NULL
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 3\n")

FindString <- " TREATMENT X READER ANOVA of AUCs"
offSet <- 6
nRows <- 3
rowNames <- c("T","R","TR")
nCols <- 1:3
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df4 <- ret$df

theirs <- df4;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$ANOVA$TRanova;colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 4\n")

FindString <- " READER ANOVAs of AUCs for each treatment"
offSet <- 7
nRows <- 1
rowNames <- "R"
nCols <- 1:3
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df5<- ret$df

theirs <- df5[2:3];colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$ANOVA$IndividualTrt[,2];colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.vector(mine)
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 5\n")

FindString <- " *****        Variance component and error-covariance estimates        *****"
offSet <- 9
nRows <- 6
nCols <- 1
rowNames <- c("Var(R)","Var(TR)","COV1","COV2","COV3","Var(Error)")
nCols <- 1
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df6<- ret$df

theirs <- as.vector(df6)#;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$ANOVA$VarCom;colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)[,1]
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 6\n")

FindString <- " Corresponding DBM variance component and covariance estimates"
offSet <- 4
nRows <- 6
nCols <- 1
rowNames <- c("Var(R)","Var(C)","Var(T*R)","Var(T*C)","Var(R*C)","Var(T*R*C) + Var(Error)")
nCols <- 1
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df7<- ret$df

theirs <- as.vector(df7)#;colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- DBM$ANOVA$VarCom;colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)[,1]
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 7\n")

FindString <- " *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****"
offSet <- 9
nRows <- 1
nCols <- 1
rowNames <- "Treatment"
nCols <- 1:4
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df8<- ret$df

theirs <- as.matrix(df8);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRRC$FTests[1,];colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.01, scale = abs(mine))

# cat("passed test 8\n")

FindString <- ""
offSet <- 0
nRows <- 1
nCols <- 2
rowNames <- "Error term"
nCols <- 1:2
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df9<- ret$df

theirs <- as.matrix(df9);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRRC$FTests[2,][1:2];colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 9\n")

FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
offSet <- 6
nRows <- 1
nCols <- 7
rowNames <- "1 - 2"
nCols <- 1:7
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df10<- ret$df

theirs <- as.matrix(df10);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRRC$ciDiffTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 10\n")

FindString <- "    c) Single-treatment 95% confidence intervals"
offSet <- 6
nRows <- 2
nCols <- 6
rowNames <- c("1", "2")
nCols <- 1:6
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df11<- ret$df

theirs <- as.matrix(df11);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRRC$ciAvgRdrEachTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 11\n")

FindString <- "    a) Chi-square test for H0: Treatments have the same AUC"
offSet <- 7
nRows <- 1
nCols <- 3
rowNames <- ""
nCols <- 1:3
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df12<- ret$df

theirs <- as.matrix(df12);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$FRRC$FTests[1,2:4]
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 12\n")

FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
offSet <- 6
nRows <- 1
nCols <- 6
rowNames <- "1 - 2"
nCols <- 1:6
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df13<- ret$df

theirs <- as.matrix(df13);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$FRRC$ciDiffTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# their program does not print out as many decimal places
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 13\n")

FindString <- "    c) Single treatment AUC 95% confidence intervals"
offSet <- 6
nRows <- 2
nCols <- 4
rowNames <- c("1", "2")
nCols <- 1:4
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df14<- ret$df

theirs <- as.matrix(df14);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$FRRC$ciAvgRdrEachTrt[,-3] # drop DF
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))

# cat("passed test 14\n")

FindString <- "  Treatment  Var(Error)     Cov2   "
offSet <- 2
nRows <- 2
nCols <- 2
rowNames <- c("1", "2")
nCols <- 1:2
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df15<- ret$df

theirs <- as.matrix(df15);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$ANOVA$IndividualTrt[,-c(1,2)] # drop DF and msREachTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))

# cat("passed test 15\n")

FindString <- "    d) Single-reader 95% confidence intervals and tests (H0: difference = 0) for "
offSet <- 8
nRows <- 5
nCols <- 6
rowNames <- c("1", "2", "3", "4", "5", "6")
nCols <- 1:6
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df16<- ret$df

theirs <- as.matrix(df16);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$FRRC$ciDiffTrtEachRdr
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# insufficient decimal places in theirs
expect_equal(theirs, mine, tolerance = 0.01, scale = abs(mine))

# cat("passed test 16\n")

FindString <- " Source        DF    Mean Square      F value  Pr > F "
offSet <- 2
nRows <- 1
nCols <- 4
rowNames <- "Treatment"
nCols <- 1:4
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df17<- ret$df

theirs <- as.matrix(df17);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRFC$FTests[1,]
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
# insufficient decimal places in theirs
expect_equal(theirs, mine, tolerance = 0.005, scale = abs(mine))

# cat("passed test 17\n")

FindString <- ""
offSet <- 0
nRows <- 1
nCols <- 7
rowNames <- "1 - 2"
nCols <- 1:7
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df18<- ret$df

theirs <- as.vector(df18[1:2]);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRFC$FTests[2,1:2]
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.numeric(mine)
expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))

# cat("passed test 18\n")

FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
offSet <- 6
nRows <- 1
nCols <- 7
rowNames <- "1 - 2"
nCols <- 1:7
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df19<- ret$df

theirs <- as.vector(df19);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRFC$ciDiffTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.numeric(mine)
# insufficient precision printout in theirs
expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))

# cat("passed test 19\n")

FindString <- "    c) Single treatment AUC 95% confidence intervals"
offSet <- 6
nRows <- 2
nCols <- 6
rowNames <- c("1" ,"2")
nCols <- 1:6
ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
CurrentLine <- ret$CurrentLine
df20<- ret$df

theirs <- as.matrix(df20[,-2]);colnames(theirs) <- NULL;rownames(theirs) <- NULL
mine <- OR$RRFC$ciAvgRdrEachTrt
colnames(mine) <- NULL;rownames(mine) <- NULL
mine <- as.matrix(mine)
expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))

# cat("passed test 20\n")

})

contextStr <- "StSignificanceTesting: Compare to Iowa Franken dataset"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/Iowa/Franken1.txt")
  lines <- readLines(fn)
  
  DBM <- StSignificanceTesting(dataset03, FOM = "Wilcoxon", method = "DBM")
  OR <- StSignificanceTesting(dataset03, FOM = "Wilcoxon", method = "OR")
  
  CurrentLine <- 1
  FindString <- "TREATMENT x READER AUC ESTIMATES"
  offSet <- 6
  nRows <- 4
  rowNames <- as.character(seq(1:nRows))
  nCols <- c(1,2)
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df1 <- ret$df
  
  theirs <- df1;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- t(DBM$FOMs$foms);colnames(mine) <- NULL;rownames(mine) <- NULL
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  FindString <- " TREATMENT AUC MEANS (averaged across readers)"
  offSet <- 2
  nRows <- 2
  rowNames <- as.character(seq(1:2))
  nCols <- 1
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df2 <- ret$df
  
  theirs <- df2;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- as.matrix(DBM$FOMs$trtMeans);colnames(mine) <- NULL;rownames(mine) <- NULL
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 2\n")
  
  FindString <- " TREATMENT AUC MEAN DIFFERENCES"
  offSet <- 2
  nRows <- 1
  rowNames <- "1 - 2"
  nCols <- 1
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df3 <- ret$df
  
  theirs <- df3;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- as.matrix(DBM$FOMs$trtMeanDiffs);colnames(mine) <- NULL;rownames(mine) <- NULL
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 3\n")
  
  FindString <- " TREATMENT X READER ANOVA of AUCs"
  offSet <- 6
  nRows <- 3
  rowNames <- c("T","R","TR")
  nCols <- 1:3
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df4 <- ret$df
  
  theirs <- df4;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$ANOVA$TRanova;colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 4\n")
  
  FindString <- " READER ANOVAs of AUCs for each treatment"
  offSet <- 7
  nRows <- 1
  rowNames <- "R"
  nCols <- 1:3
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df5<- ret$df
  
  theirs <- df5[2:3];colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$ANOVA$IndividualTrt[,2];colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.vector(mine)
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 5\n")
  
  FindString <- " *****        Variance component and error-covariance estimates        *****"
  offSet <- 9
  nRows <- 6
  nCols <- 1
  rowNames <- c("Var(R)","Var(TR)","COV1","COV2","COV3","Var(Error)")
  nCols <- 1
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df6<- ret$df
  
  theirs <- as.vector(df6)#;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$ANOVA$VarCom;colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)[,1]
  # first two elements are expected to be unequal - see NEWS.md 5/26/20
  expect_error(expect_equal(theirs[1:2], mine[1:2], tolerance = 0.00001, scale = abs(mine)))
  #expect_equal(theirs[3:6], mine[3:6], tolerance = 0.00001, scale = abs(mine)) # original pre 7/19/22
  expect_equal(theirs[3:6], mine[3:6], tolerance = 0.00001) # 7/19/22 weird error fixed by this change
  # Warning message:
  #   In abs(target - current)/(N * scale) :
  #   longer object length is not a multiple of shorter object length
  
  # cat("passed test 6\n")
  
  FindString <- " Corresponding DBM variance component and covariance estimates"
  offSet <- 4
  nRows <- 6
  nCols <- 1
  rowNames <- c("Var(R)","Var(C)","Var(T*R)","Var(T*C)","Var(R*C)","Var(T*R*C) + Var(Error)")
  nCols <- 1
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df7<- ret$df
  
  theirs <- as.vector(df7)#;colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- DBM$ANOVA$VarCom;colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)[,1]
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 7\n")
  
  FindString <- " *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****"
  offSet <- 9
  nRows <- 1
  nCols <- 1
  rowNames <- "Treatment"
  nCols <- 1:4
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df8<- ret$df
  
  theirs <- as.matrix(df8);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRRC$FTests[1,];colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.01, scale = abs(mine))
  
  # cat("passed test 8\n")
  
  FindString <- ""
  offSet <- 0
  nRows <- 1
  nCols <- 2
  rowNames <- "Error term"
  nCols <- 1:2
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df9<- ret$df
  
  theirs <- as.matrix(df9);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRRC$FTests[2,][1:2];colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))
  
  # cat("passed test 9\n")
  
  FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
  offSet <- 6
  nRows <- 1
  nCols <- 7
  rowNames <- "1 - 2"
  nCols <- 1:7
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df10<- ret$df
  
  theirs <- as.matrix(df10);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRRC$ciDiffTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.004, scale = abs(mine))
  
  # cat("passed test 10\n")
  
  FindString <- "    c) Single-treatment 95% confidence intervals"
  offSet <- 6
  nRows <- 2
  nCols <- 6
  rowNames <- c("1", "2")
  nCols <- 1:6
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df11<- ret$df
  
  theirs <- as.matrix(df11);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRRC$ciAvgRdrEachTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))
  
  # cat("passed test 11\n")
  
  FindString <- "    a) Chi-square test for H0: Treatments have the same AUC"
  offSet <- 7
  nRows <- 1
  nCols <- 3
  rowNames <- ""
  nCols <- 1:3
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df12<- ret$df
  
  theirs <- as.matrix(df12);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$FRRC$FTests[1,2:4]
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.001, scale = abs(mine))
  
  # cat("passed test 12\n")
  
  FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
  offSet <- 6
  nRows <- 1
  nCols <- 6
  rowNames <- "1 - 2"
  nCols <- 1:6
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df13<- ret$df
  
  theirs <- as.matrix(df13);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$FRRC$ciDiffTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # their program does not print out as many decimal places
  expect_equal(theirs, mine, tolerance = 0.004, scale = abs(mine))
  
  # cat("passed test 13\n")
  
  FindString <- "    c) Single treatment AUC 95% confidence intervals"
  offSet <- 6
  nRows <- 2
  nCols <- 4
  rowNames <- c("1", "2")
  nCols <- 1:4
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df14<- ret$df
  
  theirs <- as.matrix(df14);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$FRRC$ciAvgRdrEachTrt[,-3] # drop DF
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  expect_equal(theirs, mine, tolerance = 0.00001, scale = abs(mine))
  
  # cat("passed test 14\n")
  
  FindString <- "  Treatment  Var(Error)     Cov2   "
  offSet <- 2
  nRows <- 2
  nCols <- 2
  rowNames <- c("1", "2")
  nCols <- 1:2
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df15<- ret$df
  
  theirs <- as.matrix(df15);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$ANOVA$IndividualTrt[,-c(1,2)] # drop DF and msREachTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))
  
  # cat("passed test 15\n")
  
  FindString <- "    d) Single-reader 95% confidence intervals and tests (H0: difference = 0) for "
  offSet <- 8
  nRows <- 4
  nCols <- 6
  rowNames <- c("1", "2", "3", "4", "5")
  nCols <- 1:6
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df16<- ret$df
  
  theirs <- as.matrix(df16);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$FRRC$ciDiffTrtEachRdr
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # insufficient decimal places in theirs
  expect_equal(theirs, mine, tolerance = 0.01, scale = abs(mine))
  
  # cat("passed test 16\n")
  
  FindString <- " Source        DF    Mean Square      F value  Pr > F "
  offSet <- 2
  nRows <- 1
  nCols <- 4
  rowNames <- "Treatment"
  nCols <- 1:4
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df17<- ret$df
  
  theirs <- as.matrix(df17);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRFC$FTests[1,]
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  # insufficient decimal places in theirs
  expect_equal(theirs, mine, tolerance = 0.005, scale = abs(mine))
  
  # cat("passed test 17\n")
  
  FindString <- ""
  offSet <- 0
  nRows <- 1
  nCols <- 7
  rowNames <- "1 - 2"
  nCols <- 1:7
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df18<- ret$df
  
  theirs <- as.vector(df18[1:2]);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRFC$FTests[2,1:2]
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.numeric(mine)
  expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))
  
  # cat("passed test 18\n")
  
  FindString <- "    b) 95% confidence intervals and hypothesis tests (H0: difference = 0)"
  offSet <- 6
  nRows <- 1
  nCols <- 7
  rowNames <- "1 - 2"
  nCols <- 1:7
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df19<- ret$df
  
  theirs <- as.vector(df19);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRFC$ciDiffTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.numeric(mine)
  # insufficient precision printout in theirs
  expect_equal(theirs, mine, tolerance = 0.004, scale = abs(mine))
  
  # cat("passed test 19\n")
  
  FindString <- "    c) Single treatment AUC 95% confidence intervals"
  offSet <- 6
  nRows <- 2
  nCols <- 6
  rowNames <- c("1" ,"2")
  nCols <- 1:6
  ret <- ExtractDataFrame(lines, FindString, offSet, nRows, rowNames, CurrentLine, nCols)
  CurrentLine <- ret$CurrentLine
  df20<- ret$df
  
  theirs <- as.matrix(df20[,-2]);colnames(theirs) <- NULL;rownames(theirs) <- NULL
  mine <- OR$RRFC$ciAvgRdrEachTrt
  colnames(mine) <- NULL;rownames(mine) <- NULL
  mine <- as.matrix(mine)
  expect_equal(theirs, mine, tolerance = 0.000001, scale = abs(mine))
  
  # cat("passed test 20\n")
  
})

