library(survival)
library(shiny)
load("validation.data.mRNA.RData")
load("GI.interactions.RData")
clin <- clinical[,17:18]
rm(clinical)
data <- cbind2(data.matrix(clin),t(measurements))
rm(measurements)
interactions <- dataset$states
rm(dataset)
formula <- with(clin,Surv(time,status==1))
fullKm <- survfit(formula~1,data=clin,conf.type="log-log")
rm(formula)
runApp("../CancerApp",host="128.8.132.200",port=2017)
