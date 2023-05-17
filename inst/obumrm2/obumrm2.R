rm(list = ls())

library(RJafroc)
library(testthat)

source("inst/obumrm2/covariance.R")

FOM <- "Wilcoxon"

ds <- RJafroc::dataset02

st <- StSignificanceTesting(ds, FOM = FOM, method = "OR")

I <- dim(ds$ratings$NL)[1]
J <- dim(ds$ratings$NL)[2]

COV <- covariance(ds, FOM = FOM)

accur <- UtilFigureOfMerit(ds, FOM = FOM)

#	COMPUTE THE P-VALUES FOR EACH READER FOR EACH MODALITY PAIR

p_read <- array(dim = c(I,I,J))
special  <- 0
for (j in 1:J) {
  for (i in 1:I) {
    special <- special + COV[i,i,j,j]
    for (ip in 1:I) {
      if (ip > i) {
        num_zz <- accur[i,j]-accur[ip,j]
        den_zz <- sqrt(COV[i,i,j,j] + COV[ip,ip,j,j] - 2 * COV[i,ip,j,j])
        zz <- num_zz/den_zz
        p_read[i,ip,j]= 2 * pnorm(zz)
        cat(j,COV[i,i,j,j],COV[ip,ip,j,j],COV[i,ip,j,j],"\n")
      }
    }
  }
}

special  <- special/(I * J)
cat("\n")
for (j in 1:J) {
  cat(j,accur[1,j],sqrt(COV[1,1,j,j]),accur[2,j],
      + sqrt(COV[2,2,j,j]),p_read[1,2,j], "\n")
}

#	ESTIMATE THE MEAN FOR EACH MODALITY
mean_t <- array(dim = I)
for (i in 1:I) {
  sum <- 0
  for (j in 1:J) {
    sum <- sum + accur[i,j]
  }
  mean_t[i] <- sum/J
}

#	ESTIMATE THE MEAN FOR EACH READER
mean_r <- array(dim = J)
for (j in 1:J) {
  sum <- 0
  for (i in 1:I) {
    sum <- sum + accur[i,j]
  }
  mean_r[j] <- sum/I
}

#	ESTIMATE THE OVERALL MEAN

sum <- 0
for (i in 1:I) {
  sum <- sum + mean_t[i]
}
mean_o <- sum/I 
expect_equal(mean_o, mean(unlist(st$FOMs$foms)) )

# ESTIMATE R_1
sum <- 0
sum2 <- 0
minus <- 0
count <- 0
for (i in 1:I) {
  for (ip in 1:I) {
    if (ip > i) {
      for (j in 1:J) {
        if((COV[i,i,j,j] == 0) || (COV[ip,ip,j,j] == 0)) {
          minus <- minus+1
          next
        }
        sum <- sum + COV[i,ip,j,j]/sqrt(COV[i,i,j,j]*COV[ip,ip,j,j])
        sum2 <- sum2 + COV[i,ip,j,j]
        count <- count + 1
      }
    }
  }
}
r1 <- sum/(I*J*(I-1)/2)
covr1 <- sum2/((I*J*(I-1)-minus)/2)
expect_equal(st$ANOVA$VarCom$Estimates[3], covr1)
# but correlations are different

#	ESTIMATE R_2
sum <- 0
sum2 <- 0
count <- 0
for (i in 1:I) {
  for (j in 1:J) {
    for (jp in 1:J) {
      if (jp > j) {
        sum <- sum + COV[i,i,j,jp]/sqrt(COV[i,i,j,j]*COV[i,i,jp,jp])
        sum2 <- sum2 + COV[i,i,j,jp]
        count <- count + 1
      }
    }
  }
}
r2 <- sum / count
covr2 <- sum2 / count
expect_equal(st$ANOVA$VarCom$Estimates[4], covr2)
# but correlations are different


#	ESTIMATE R_3
sum <- 0
sum2 <- 0
count <- 0
for (i in 1:I) {
  for (ip in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if((ip != i) && (jp != j)) {
          sum <- sum + COV[i,ip,j,jp]/sqrt(COV[i,i,j,j]*COV[ip,ip,jp,jp])
          sum2 <- sum2 + COV[i,ip,j,jp]
          count <- count + 1
        }
      }
    }
  }
}
r3 <- sum/count
covr3 <- sum2/count
expect_equal(st$ANOVA$VarCom$Estimates[5], covr3)
# but correlations are different

#	COMPARE R_2 AND R_3
if(r3 > r2) {
  tempm <- (r2+r3)/2
  r2 <- tempm
  r3 <- tempm
}

if(covr3 > covr2) {
  tempm <- (covr2+covr3)/2.0
  covr2 <- tempm
  covr3 <- tempm
}

#	ESTIMATE SIGMAB and RB
sum1 <- 0
sum2 <- 0
sum3 <- 0
sum12 <- 0
sum13 <- 0
sum23 <- 0

for (j in 1:J) {
  sum1 <- sum1+(accur[1,j]-mean_t[1])**2
  sum2 <- sum2+(accur[2,j]-mean_t[2])**2
  sum12 <- sum12+(accur[1,j]-mean_t[1])*(accur[2,j]-mean_t[2])
}

rb <- sum12/sqrt(sum1*sum2)
sigmab <- (sum1+sum2)/((J-1)*2)

#	CALCULATE THE DENOMINATOR OF FSTAR (equation 3.13) (Random-Reader)
sum <- 0
for (i in 1:I) {
  for (j in 1:J) {
    sum <- sum + (accur[i,j] - mean_t[i] - mean_r[j] + mean_o)**2
  }
}
sum <- sum/((I-1)*(J-1))
den_f <- sum + J*(covr2-covr3)

#	CALCULATE THE NUMERATOR OF FSTAR (Random-Reader)
sum <- 0
for (i in 1:I) {
  sum <- sum + (mean_t[i] - mean_o)**2
}
num_f <- J*sum/(I-1)

#	CALCULATE FSTAR 
fstar <- num_f/den_f

#	GET THE P-VALUE OF FSTAR (Random-Reader) Nancy's original idea
dfn <- I-1
dfd <- (I-1)*(J-1)
p_fstar <- 1 - pf(fstar, dfn, dfd)

# Nancy code for sum of squares etc
correct <- 0
totalss <- 0
for (i in 1:I) {
  for (j in 1:J) {
    correct <- correct + accur[i,j]
    totalss <- totalss + accur[i,j]*accur[i,j]
  }
}

correct <-  correct*correct/(I*J)
totalss <- totalss - correct

trtsum <- array(0, dim = I)
readersum <- array(0, dim = J)

for (i in 1:I) {
  for (j in 1:J) {
    trtsum[i] <- trtsum[i] + accur[i,j]
    readersum[j] <- readersum[j] + accur[i,j]
  }
}

trtsss <- 0
for (i in 1:I) {
  trtsss <- trtsss + trtsum[i] * trtsum[i]
}
trtsss <- trtsss/J - correct

readss <- 0
for (j in 1:J) {
  readss <- readss + readersum[j]*readersum[j]
}
readss <- readss/I - correct

ss_inter <- totalss - trtsss - readss
ms_inter <- ss_inter/((I-1)*(J-1))

#	Estimate below due to Steve Hillis 09/05/2003
v_inter <- ms_inter - (special - covr1 - covr2 + covr3)

#	Revisions made on April 7, 2005 to accommodate the DFs proposed
#	by Steve Hillis at ENAR 2005.
#	GET THE P-VALUE OF FSTAR (Random-Reader)
dfn <- I-1
if((covr2-covr3) > 0) {
  dfd <- ((ms_inter + J*(covr2-covr3))**2)/((ms_inter**2)/((I-1)*(J-1)))
}

if((covr2-covr3) <= 0) {
  dfd <- ((ms_inter)**2)/((ms_inter**2)/((I-1)*(J-1)))
}

p_fstar_H <- 1 - pf(fstar, dfn, dfd) # OK with RJafroc, see next line
expect_equal(st$RRRC$FTests$p[1], p_fstar_H)
