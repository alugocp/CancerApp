library(shiny)
load("validation.data.mRNA.RData")
shinyUI(fluidPage(
	theme="style.css",
	onmouseup="selected=null",
	tags$title("Alex Lugo's Cancer App"),
	fluidRow(
		id="header",
		column(4,
			id="logoColumn",
			tags$h2("Cancer App",id="logo",title="Super G-Type 2 font by gomarice on 1001fonts.com"),
			tags$a("by Alex Lugo",href="http://alugocp.github.io/resume",title="Click to access Alex's resume")
		),
		column(4,
			selectizeInput("searched","Search genes...",genes[0:100])
		),
		column(4,
			numericInput("quantile","Pick a K value",0.3,step=0.05,min=0.1,max=0.5)
		)
	),
	fluidRow(
		mainPanel(
			tags$canvas(id="canvas",width=200,height=200,onclick="clickCanvas()",onmousedown="mousedown(event)",onmousemove="mousemove(event)"),
			tags$br(),
			tags$br(),
			tags$br(),
			plotOutput("km"),
			textOutput("nodeData"),
			tags$script(src="frontEnd.js")
		)
	)
))