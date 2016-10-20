library(shiny)
shinyUI(fluidPage(
	onmouseup="selected=null",
	tags$title("Alex Lugo's Kaplan-Meier Estimator"),
	fluidRow(
		column(4,
			tags$h2("Cancer App")
		),
		column(4,
			selectizeInput("searched","Search genes...",c("age","sex","wt.loss","meal.cal","ph.ecog","ph.karno","pat.karno","inst"))
		),
		column(4,
			numericInput("quantile","Pick a K value",0.1,step=0.05,min=0.1,max=0.5)
		)
	),
	fluidRow(
		mainPanel(
			tags$canvas(id="canvas",width=200,height=200,onclick="clickCanvas()",onmousedown="mousedown(event)",onmousemove="mousemove(event)"),
			plotOutput("km"),
			textOutput("nodeData"),
			tags$script(src="frontEnd.js")
		)
	)
))