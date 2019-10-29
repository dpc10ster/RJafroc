
```{r}
a<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
effectSizeROC <- seq(0.01, 0.1, 0.001)
effectSizewAFROC <- effectSizeROC*a$coefficients[1] # r2 = summary(a)$r.squared
````



```{r}
varCompROC <- StSignificanceTesting(rocData, FOM = "Wilcoxon", method = "DBMH", option = "RRRC")$varComp
varCompwAFROC <- StSignificanceTesting(frocData, FOM = "wAFROC", method = "DBMH", option = "RRRC")$varComp
````



```{r}
powerROC <- array(dim = length(effectSizeROC));powerwAFROC <- array(dim = length(effectSizeROC))

JTest <- 5;KTest <- 100
for (i in 1:length(effectSizeROC)) {
  varYTR <- varCompROC$varTR
  varYTC <- varCompROC$varTC
  varYEps <- varCompROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizeROC[i], varYTR, varYTC, varYEps, alpha  = 0.05, option = "RRRC")
  powerROC[i] <- ret$powerRRRC
  
  varYTR <- varCompwAFROC$varTR
  varYTC <- varCompwAFROC$varTC
  varYEps <- varCompwAFROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizewAFROC[i], varYTR, varYTC, varYEps, alpha  = 0.05, option = "RRRC")
  powerwAFROC[i] <- ret$powerRRRC
  
  cat("ROC-ES = ", effectSizeROC[i], ", wAFROC-ES = ", effectSizewAFROC[i], 
      ", Power-ROC = ", powerROC[i], ", Power-wAFROC = ", powerwAFROC[i], "\n")
}
````



````{r, fig.show='hold', fig.align='center'}
df <- data.frame(esRoc = esRoc, eswAfroc = eswAfroc)
p <- ggplot(data = df, aes(x = esRoc, y = eswAfroc)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  geom_point(size = 4) +
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 25,face="bold"),
        axis.title.x = element_text(size = 30,face="bold")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 
print(p)
````             





````{r, fig.show='hold', fig.align='center'}
df <- data.frame(powerROC = powerROC, powerwAFROC = powerwAFROC)
p <- ggplot(mapping = aes(x = powerROC, y = powerwAFROC)) +
  geom_line(data = df, size = 2)+
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 25,face="bold"),
        axis.title.x = element_text(size = 30,face="bold"))  +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
````             

