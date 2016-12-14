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
		
		rm(d,clin,km,bins)
		legend(1,0.4,c("low","med","high"),col=c("red","blue","green"),lty=c(1,1))
	}
	graphEdge <- function(gene,gene1){
		i <- match(gene,genes)
		i1 <- match(gene1,genes)
		bins <- (bin.map[i,]*3)+bin.map[i1,]+1
		flipbins <- (bin.map[i1,]*3)+bin.map[i,]+1
		graphData(paste(gene," vs. ",gene1))
		
		bin <- data.frame(subset(data,bins[]==1))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="red")
		
		bin <- data.frame(subset(data,bins[]==2))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="yellow")
		
		bin <- data.frame(subset(data,bins[]==3))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="orange")
		
		bin <- data.frame(subset(data,flipbins[]==2))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="purple")
		
		bin <- data.frame(subset(data,bins[]==5))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="blue")
		
		bin <- data.frame(subset(data,bins[]==6))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="pink")
		
		bin <- data.frame(subset(data,flipbins[]==3))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="gray")
		
		bin <- data.frame(subset(data,flipbins[]==6))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="cyan")
		
		bin <- data.frame(subset(data,bins[]==9))
		clin <- with(bin,Surv(time,status==1))
		km <- survfit(clin~1,data=bin,conf.type="log-log")
		lines(km,conf.int=F,mark.time=F,col="green")
		
		rm(bin,clin,km,bins,flipbins)
		legend(1,0.6,c("low-low","low-med","low-high","med-low","med-med","med-high","high-low","high-med","high-high"),col=c("red","yellow","orange","purple","blue","pink","gray","cyan","green"),lty=c(1,1))
	}
	graphData <- function(label){
		plot(fullKm,conf.int=F,mark.time=F,xlab="Time (Days)",ylab="Chance of Survival",main=label)
	}
	getRole <- function(index){
		i <- match(genes[index],roles[,1])
		if(!is.na(i)){
			return(roles[i,2])
		}
		return("none")
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
				x <- paste(searched[s],getRole(index),"true",sep=",")
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
						x1 <- paste(genes[index1],getRole(index1),"false",sep=",")
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
