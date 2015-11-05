#setwd("~/Documents/Study/Hof/TextBook/MKT280/HomeWork/ex4")
library(lavaan)
library(qgraph)
library(semPlot)
library(semTools)
data = read.csv("CE.csv")
colnames(data)=c("CE1_1","CE1_2","CE1_3","CE1_4","CE1_5","CE1_6","CE1_7","CE1_8","CE1_9","CE1_10","CE1_11","CE1_12","CE1_13",
                 "CE2_1","CE2_2","CE2_3","CE2_4","CE2_5","CE2_6","CE2_7",
                 "RI1","RI2","RI3","RI4","CP1","CP2","CP3","CP4","CP5","CP6","CP7","EX1","EX2","EX3","EX4","EX5")
##X and Y
#x =c("CE1_1","CE1_2","CE1_3","CE1_4","CE1_5","CE1_6","CE1_7","CE1_8","CE1_9","CE1_10","CE1_11","CE1_12","CE1_13","CE2_1","CE2_2","CE2_3","CE2_4","CE2_5","CE2_6","CE2_7")
#cat(x,sep="+")
# RI= c("RI1","RI2","RI3","RI4")
# cat(RI,sep="+")
# CP = c("CP1","CP2","CP3","CP4","CP5","CP6","CP7")
# cat(CP,sep="+")
# EX = c("EX1","EX2","EX4","EX5")
# cat(EX,sep="+")
##Model
model = '
  #measurement model
  CE1 =~ CE1_1+CE1_2+CE1_3+CE1_4+CE1_5+CE1_6+CE1_7+CE1_8+CE1_9+CE1_10+CE1_11+CE1_12+CE1_13
  CE2 =~ CE2_1+CE2_2+CE2_3+CE2_4+CE2_5+CE2_6+CE2_7
  RI =~ RI1+RI2+RI3+RI4
  CP =~ CP1+CP2+CP3+CP4+CP5+CP6+CP7
  EX =~ EX1+EX2+EX4+EX5
  #regressions
  RI ~ CE1 + CE2 + CP 
  CP ~ CE1 + CE2 + EX
  EX ~ CE1 + CE2 + RI 
  #residual correlations
  RI1~~CP1
  RI2~~CP2
  RI3~~CP3
  RI4~~CP4
'
##fit your SEM
fit = sem(model,data=data)
##Summarize results
summary(fit, fit.measure=T,standardized=TRUE,rsq=T)
summary(fit,fit.measure=T, standardized=TRUE)
##plot result using semPaths function in qgraph
semPaths(fit,"std",edge.label.cex=0.5,curvePivot=TRUE, layout="tree")
semPaths(fit,"par",layout="spring")

###Check to see if you missed anythong, High mi values suggest that threre is a path that you missed
modindices(fit) 

## looks good
#can also look at variance tables
vartable(fit)

###Relibility
reliability(fit)

##index
inspect(fit,what="start")

##fitmeasures
fitMeasures(fit)

##modificationIndices
modin = modificationIndices(fit)

###coef
coef(fit)

####AVE
summary(fit, fit.measure=T,standardized=TRUE,rsq=T)
modin = modin[order(modin$mi,decreasing=T),]
###remove the path which has a high mi