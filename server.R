library(shiny)
library(survival)
#data <- read.table(pipe("ssh alugocp@ibissub01@umiacs.umd.edu '../../cbcb/project2-scratch/gi_visualization/validation.data.mRNA.RData'"),header=T)
shinyServer(function(input,output){
	output$km <- renderPlot({
		tryCatch({
			if(input$gene1=="none"){
				graphNode(input$quantile,input$gene)
			}else{
				graphEdge(input$quantile,input$gene,input$gene1)
			}
		},error=function(e){
			graphData("")
		})
	})
	graphNode <- function(k,gene){
		data <- lung
		cutoffs <- quantile(data[,gene],c(k,1-k),na.rm=TRUE)#na.rm=TRUE allows for N/A data. Remove later?
		graphData(gene)
		
		low <- subset(data,data[gene]<=cutoffs[1])
		low$SurvObj <- with(low,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=low,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="red")
		
		high <- subset(data,data[gene]>=cutoffs[2])
		high$SurvObj <- with(high,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=high,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="green")
		
		legend(850,1,c("low","high"),col=c("red","green"),lty=c(1,1))
	}
	graphEdge <- function(k,gene,gene1){
		data <- lung
		cutoffs <- c(quantile(data[,gene],c(k,1-k),na.rm=TRUE),quantile(data[,gene1],c(k,1-k),na.rm=TRUE))
		graphData(c(gene," vs. ",gene1))
		
		lowlow <- subset(data,data[gene]<=cutoffs[1] & data[gene1]<=cutoffs[3])
		lowlow$SurvObj <- with(lowlow,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=lowlow,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="red")
		
		lowhigh <- subset(data,data[gene]<=cutoffs[1] & data[gene1]>=cutoffs[4])
		lowhigh$SurvObj <- with(lowhigh,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=lowhigh,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="green")
		
		highhigh <- subset(data,data[gene]>=cutoffs[2] & data[gene1]>=cutoffs[4])
		highhigh$SurvObj <- with(highhigh,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=highhigh,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="blue")
		
		highlow <- subset(data,data[gene]>=cutoffs[2] & data[gene1]<=cutoffs[3])
		highlow$SurvObj <- with(highlow,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=highlow,conf.type="log-log")
		lines(km,conf=F,mark.time=F,col="orange")
		
		legend(850,1,c("low-low","low-high","high-high","high-low"),col=c("red","green","blue","orange"),lty=c(1,1))
	}
	graphData <- function(label){
		data <- lung
		data$SurvObj <- with(data,Surv(time,status==2))
		km <- survfit(SurvObj~1,data=data,conf.type="log-log")
		plot(km,conf=F,mark.time=F,xlab="Time",ylab="Chance of Survival",main=label)
	}
	output$nodeData1 <- renderPrint({
		"App"
	})
	output$nodeData <- renderPrint({
		gene <- input$searched
		table <- list(
			c("age","wt.loss","meal.cal","sex"),
			c("wt.loss","age","meal.cal","sex"),
			c("meal.cal","age","wt.loss","sex"),
			c("sex","age","wt.loss","meal.cal","inst","ph.karno","pat.karno","ph.ecog"),
			c("pat.karno","ph.karno","ph.ecog","inst","sex"),
			c("ph.karno","pat.karno","ph.ecog","inst","sex"),
			c("ph.ecog","pat.karno","ph.karno","inst","sex"),
			c("inst","pat.karno","ph.karno","ph.ecog","sex")
		)
		for(node in table){
			if(node[1]==gene){
				print(node)
				for(i in 2:length(node)){
					for(node1 in table){
						if(node1[1]==node[i]){
							print(node1)
						}
					}
				}
				break
			}
		}
	})
})