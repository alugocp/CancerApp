library(shiny)
library(survival)
shinyServer(function(input,output){
	load("validation.data.mRNA.RData")
	clin <- clinical[,17:18]#data.frame(cbind2(clinical[,"time"],clinical[,"status"]))
	#colnames(clin) <- c("time","status")
	data <- cbind2(data.matrix(clin),t(measurements))
	output$km <- renderPlot({
		if(is.null(input$gene)){
			graphData("")
		}else{
			if(input$gene1=="none"){
				graphNode(input$quantile,input$gene)
			}else{
				graphEdge(input$quantile,input$gene,input$gene1)
			}
		}
	})
	graphNode <- function(k,gene){
		i <- match(gene,genes)
		cutoffs <- quantile(data[,i+2],c(k,1-k),na.rm=TRUE)
		graphData(gene)
		
		low <- data.frame(subset(data,data[,i+2]<=cutoffs[1])[,1:2])
		lowClin <- with(low,Surv(time,status==1))
		km <- survfit(lowClin~1,data=low,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		high <- data.frame(subset(data,data[,i+2]>=cutoffs[2])[,1:2])
		highClin <- with(high,Surv(time,status==1))
		km <- survfit(highClin~1,data=high,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		legend(850,1,c("low","high"),col=c("red","green"),lty=c(1,1))
	}
	graphEdge <- function(k,gene,gene1){
		i <- match(gene,genes)
		i1 <- match(gene1,genes)
		cutoffs <- c(quantile(data[,i+2],c(k,1-k),na.rm=TRUE),quantile(data[,i1+2],c(k,1-k),na.rm=TRUE))
		graphData(c(gene," vs. ",gene1))
		
		lowlow <- data.frame(subset(data,data[,i+2]<=cutoffs[1] & data[,i1+2]<=cutoffs[3])[,1:2])
		lowlowClin <- with(lowlow,Surv(time,status==1))
		km <- survfit(lowlowClin~1,data=lowlow,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		lowhigh <- data.frame(subset(data,data[,i+2]<=cutoffs[1] & data[,i1+2]>=cutoffs[4])[,1:2])
		lowhighClin <- with(lowhigh,Surv(time,status==1))
		km <- survfit(lowhighClin~1,data=lowhigh,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		highhigh <- data.frame(subset(data,data[,i+2]>=cutoffs[2] & data[,i1+2]>=cutoffs[4])[,1:2])
		highhighClin <- with(highhigh,Surv(time,status==1))
		km <- survfit(highhighClin~1,data=highhigh,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		highlow <- data.frame(subset(data,data[,i+2]>=cutoffs[2] & data[,i1+2]<=cutoffs[3]))
		highlowClin <- with(highlow,Surv(time,status==1))
		km <- survfit(highlowClin~1,data=highlow,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="orange")
		
		legend(850,1,c("low-low","low-high","high-high","high-low"),col=c("red","green","blue","orange"),lty=c(1,1))
	}
	graphData <- function(label){
		formula <- with(clin,Surv(time,status==1))
		km <- survfit(formula~1,data=clin,conf.type="log-log")
		plot(km,conf.int=F,mark.time=F,xlab="Time",ylab="Chance of Survival",main=label)
	}
	output$nodeData <- renderPrint({
		load("GI.interactions.RData")
		connections <- dataset$states
		index <- match(input$searched,genes)
		con <- subset(connections,connections[,1]==index)
		x <- input$searched
		if(length(con[,1])==0){
			print(x)
		}else{
			for(i in length(con[,1])){
				x <- paste(x,",",genes[con[i,2]])
			}
			print(x)
			for(i in length(con[,1])){
				index <- con[i,2]
				con1 <- subset(connections,connections[,1]==index)
				x <- genes[index]
				if(length(con1[,1])>0){
					for(a in length(con1[,1])){
						x <- paste(x,",",genes[con1[a,2]])
					}
				}
				print(x)
			}
		}
	})
})