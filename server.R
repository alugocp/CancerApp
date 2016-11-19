library(shiny)
library(survival)
shinyServer(function(input,output){
	clin <- clinical[,17:18]
	data <- cbind2(data.matrix(clin),t(measurements))
	output$km <- renderPlot({
		if(is.null(input$gene)){
			graphData("")
		}else{
			if(input$gene1=="none"){
				graphNode(input$gene)
			}else{
				graphEdge(input$gene,input$gene1)
			}
		}
	})
	graphNode <- function(gene){
		i <- match(gene,genes)
		bins <- bin.map[i,]
		graphData(gene)
		
		low <- data.frame(subset(data,bins[]==0)[,1:2])
		lowClin <- with(low,Surv(time,status==1))
		km <- survfit(lowClin~1,data=low,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		med <- data.frame(subset(data,bins[]==1)[,1:2])
		medClin <- with(med,Surv(time,status==1))
		km <- survfit(medClin~1,data=med,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		high <- data.frame(subset(data,bins[]==2)[,1:2])
		highClin <- with(high,Surv(time,status==1))
		km <- survfit(highClin~1,data=high,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		legend(1,0.4,c("low","med","high"),col=c("red","blue","green"),lty=c(1,1))
	}
	graphEdge <- function(gene,gene1){
		i <- match(gene,genes)
		i1 <- match(gene1,genes)
		bins <- (bin.map[i,]*3)+bin.map[i1,]+1
		graphData(paste(gene," vs. ",gene1))
		
		bin1 <- data.frame(subset(data,bins[]==1))
		bin1clin <- with(bin1,Surv(time,status==1))
		km <- survfit(bin1clin~1,data=bin1,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		bin2 <- data.frame(subset(data,bins[]==2))
		bin2clin <- with(bin2,Surv(time,status==1))
		km <- survfit(bin2clin~1,data=bin2,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="yellow")
		
		bin3 <- data.frame(subset(data,bins[]==3))
		bin3clin <- with(bin3,Surv(time,status==1))
		km <- survfit(bin3clin~1,data=bin3,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="orange")
		
		bin5 <- data.frame(subset(data,bins[]==5))
		bin5clin <- with(bin5,Surv(time,status==1))
		km <- survfit(bin5clin~1,data=bin5,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		bin6 <- data.frame(subset(data,bins[]==6))
		bin6clin <- with(bin6,Surv(time,status==1))
		km <- survfit(bin6clin~1,data=bin6,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="pink")
		
		bin9 <- data.frame(subset(data,bins[]==9))
		bin9clin <- with(bin9,Surv(time,status==1))
		km <- survfit(bin9clin~1,data=bin9,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		legend(1,0.4,c("low","low-med","low-high","med","med-high","high"),col=c("red","yellow","orange","blue","pink","green"),lty=c(1,1))
	}
	graphData <- function(label){
		formula <- with(clin,Surv(time,status==1))
		km <- survfit(formula~1,data=clin,conf.type="log-log")
		plot(km,conf.int=F,mark.time=F,xlab="Time",ylab="Chance of Survival",main=label)
	}
	output$nodeData <- renderPrint({
		if(is.null(input$searched)){
			print("")
			return()
		}
		searched <- strsplit(input$searched,",")[[1]]
		binColors <- c("red","yellow","orange","","blue","pink","","","green")
		for(s in 1:length(searched)){
			index <- match(searched[s],genes)
			if(!is.na(index)){
				con <- subset(dataset$states,dataset$states[,"y"]==index)
				x <- paste(searched[s],"NULL","true",sep=",")
				if(length(con[,1])==0){
					print(x)
				}else{
					for(i in 1:length(con[,1])){
						color <- binColors[con[i,"bin"]]
						sign <- "+"
						if(con[i,4]<0){
							sign <- "-"
						}
						width <- abs(round(con[i,4],0))#*(factor/30)
						x <- paste(x,genes[con[i,"x"]],color,width,sign,sep=",")
					}
					print(x)
					for(i in 1:length(con[,1])){
						index1 <- con[i,"x"]
						con1 <- subset(dataset$states,dataset$states[,"y"]==index1)
						x1 <- paste(genes[index1],"NULL","false",sep=",")
						if(length(con1[,1])>0){
							for(a in 1:length(con1[,1])){
								color <- binColors[con1[a,"bin"]]
								sign <- "+"
								if(con1[a,4]<0){
									sign <- "-"
								}
								width <- abs(round(con1[a,4],0))
								x1 <- paste(x1,genes[con1[a,"x"]],color,width,sign,sep=",")
							}
						}
						print(x1)
					}
				}
			}
		}
	})
})