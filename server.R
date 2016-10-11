library(shiny)
library(survival)
#data <- read.table(pipe("ssh alugocp@ibissub01@umiacs.umd.edu '../../cbcb/project2-scratch/gi_visualization/validation.data.mRNA.RData'"),header=T)
shinyServer(function(input,output){
	output$km <- renderPlot({
		if(input$gene1=="none"){
			graphNode(input$quantile,input$gene)
		}else{
			graphEdge(input$quantile,input$gene,input$gene1)
		}
	})
	graphNode <- function(k,gene){
		data <- lung
		cutoffs <- quantile(data[,gene],c(k,1-k),na.rm=TRUE)#na.rm=TRUE allows for N/A data. Remove later?
		
		low <- subset(data,data[gene]<=cutoffs[1])
		low$SurvObj <- with(low,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=low,conf.type="log-log")
		plot(km,conf=F,mark.time=F,col="green")
		
		high <- subset(data,data[gene]>=cutoffs[2])
		high$SurvObj <- with(high,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=high,conf.type="log-log")
		lines(km,conf=F,mark.time=F)
	}
	graphEdge <- function(k,gene,gene1){
		data <- lung
		cutoffs <- c(quantile(data[,gene],c(k,1-k),na.rm=TRUE),quantile(data[,gene1],c(k,1-k),na.rm=TRUE))
		
		lowlow <- subset(data,data[gene]<=cutoffs[1] & data[gene1]<=cutoffs[3])
		lowlow$SurvObj <- with(lowlow,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=lowlow,conf.type="log-log")
		plot(km,conf=F,mark.time=F,col="red")
		
		lowhigh <- subset(data,data[gene]<=cutoffs[1] & data[gene1]>=cutoffs[4])
		lowhigh$SurvObj <- with(lowhigh,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=lowhigh,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="blue")
		
		highhigh <- subset(data,data[gene]>=cutoffs[2] & data[gene1]>=cutoffs[4])
		highhigh$SurvObj <- with(highhigh,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=highhigh,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="green")
		
		highlow <- subset(data,data[gene]>=cutoffs[2] & data[gene1]<=cutoffs[3])
		highlow$SurvObj <- with(highlow,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=highlow,conf.type="log-log")
		lines(km,conf=F,mark.time=F)
	}
})