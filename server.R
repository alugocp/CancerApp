library(shiny)
library(survival)
shinyServer(function(input,output){
	output$selectedGraph <- renderPlot({
    		if(input$gender==T){
    			gender()
    		}else{
    			one()
    		}
    	})
    	one <- function(){
    		lung$SurvObj <- with(lung,Surv(time,status==2))
		km <- survfit(SurvObj ~1, data = lung, conf.type = "log-log")
		plot(km,conf=F,mark.time=F)
    	}
    	gender <- function(){
    		data <- split(lung,lung$sex)
    		lung1 <- data$"1"
    		lung2 <- data$"2"
    			
    		#boys
		lung1$SurvObj <- with(lung1,Surv(time,status==2))
		km <- survfit(SurvObj ~1, data = lung1, conf.type = "log-log")
		plot(km,conf=F,mark.time=F,col="blue")
			
		#girls
		lung2$SurvObj <- with(lung2,Surv(time,status==2))
		km <- survfit(SurvObj ~ 1, data = lung2, conf.type = "log-log")
		lines(km,conf=F,mark.time=F,col="pink")
    	}
})