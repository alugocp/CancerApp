shinyServer(function(input,output){
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
		
		d <- data.frame(subset(data,bins[]==0)[,1:2])
		clin <- with(d,Surv(time,status==1))
		km <- survfit(clin~1,data=d,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		d <- data.frame(subset(data,bins[]==1)[,1:2])
		clin <- with(d,Surv(time,status==1))
		km <- survfit(clin~1,data=d,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		d <- data.frame(subset(data,bins[]==2)[,1:2])
		clin <- with(d,Surv(time,status==1))
		km <- survfit(clin~1,data=d,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		rm(d,clin,km)
		legend(1,0.4,c("low","med","high"),col=c("red","blue","green"),lty=c(1,1))
	}
	graphEdge <- function(gene,gene1){
		i <- match(gene,genes)
		i1 <- match(gene1,genes)
		bins <- (bin.map[i,]*3)+bin.map[i1,]+1
		flipbins <- (bin.map[i1,]*3)+bin.map[i,]+1
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
		
		bin4 <- data.frame(subset(data,flipbins[]==2))
		bin4clin <- with(bin4,Surv(time,status==1))
		km <- survfit(bin4clin~1,data=bin4,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="purple")
		
		bin5 <- data.frame(subset(data,bins[]==5))
		bin5clin <- with(bin5,Surv(time,status==1))
		km <- survfit(bin5clin~1,data=bin5,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		bin6 <- data.frame(subset(data,bins[]==6))
		bin6clin <- with(bin6,Surv(time,status==1))
		km <- survfit(bin6clin~1,data=bin6,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="pink")
		
		bin7 <- data.frame(subset(data,flipbins[]==3))
		bin7clin <- with(bin7,Surv(time,status==1))
		km <- survfit(bin7clin~1,data=bin7,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="gray")
		
		bin8 <- data.frame(subset(data,flipbins[]==6))
		bin8clin <- with(bin8,Surv(time,status==1))
		km <- survfit(bin8clin~1,data=bin8,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="cyan")
		
		bin9 <- data.frame(subset(data,bins[]==9))
		bin9clin <- with(bin9,Surv(time,status==1))
		km <- survfit(bin9clin~1,data=bin9,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		legend(1,0.6,c("low-low","low-med","low-high","med-low","med-med","med-high","high-low","high-med","high-high"),col=c("red","yellow","orange","purple","blue","pink","gray","cyan","green"),lty=c(1,1))
	}
	graphData <- function(label){
		plot(fullKm,conf.int=F,mark.time=F,xlab="Time",ylab="Chance of Survival",main=label)
	}
	flip <- function(bin){
		if(bin==2){
			return(4)
		}else if(bin==3){
			return(7)
		}else if(bin==6){
			return(8)
		}
		return(bin)
	}
	output$nodeData <- renderPrint({
		if(is.null(input$searched)){
			print("")
			return()
		}
		searched <- strsplit(input$searched,",")[[1]]
		binColors <- c("red","yellow","orange","purple","blue","pink","gray","cyan","green")
		for(s in 1:length(searched)){
			index <- match(searched[s],genes)
			if(!is.na(index)){
				con <- subset(interactions,interactions[,"y"]==index | interactions[,"x"]==index)
				x <- paste(searched[s],"NULL","true",sep=",")
				if(length(con[,1])==0){
					print(x)
				}else{
					for(i in 1:length(con[,1])){
						if(con[i,"y"]==index){
							connection <- genes[con[i,"x"]]
							color <- binColors[con[i,"bin"]]
						}else{
							connection <- genes[con[i,"y"]]
							color <- binColors[flip(con[i,"bin"])]
						}
						sign <- "+"
						if(con[i,4]<0){
							sign <- "-"
						}
						width <- abs(round(con[i,4],0))#*(factor/30)
						x <- paste(x,connection,color,width,sign,sep=",")
					}
					print(x)
					for(i in 1:length(con[,1])){
						if(con[i,"y"]==index){
							index1 <- con[i,"x"]
						}else{
							index1 <- con[i,"y"]
						}
						con1 <- subset(interactions,interactions[,"y"]==index1 | interactions[,"x"]==index1)
						x1 <- paste(genes[index1],"NULL","false",sep=",")
						if(length(con1[,1])>0){
							for(a in 1:length(con1[,1])){
								if(con1[a,"y"]==index1){
									connection <- genes[con1[a,"x"]]
									color <- binColors[con1[a,"bin"]]
								}else{
									connection <- genes[con1[a,"y"]]
									color <- binColors[flip(con1[a,"bin"])]
								}
								sign <- "+"
								if(con1[a,4]<0){
									sign <- "-"
								}
								width <- abs(round(con1[a,4],0))
								x1 <- paste(x1,connection,color,width,sign,sep=",")
							}
						}
						print(x1)
					}
				}
			}
		}
	})
})
