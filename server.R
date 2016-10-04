library(shiny)
shinyServer(function(input,output){
    	observeEvent(input$show,{
    		output$empires <- renderPlot({
		if(input$show==T){
    			num <- c(10,10,10,10,10)
    			emp <- c("Songhay","Incan","Roman","Byzantine","Mongol")
    			pie(num,emp)
    		}
    	})
    	})
})