library(shiny)
load("validation.data.mRNA.RData")
shinyUI(fluidPage(
	theme="style.css",
	onmouseup="selected=null",
	onresize="setCanvasDimensions()",
	tags$title("Alex Lugo's Cancer App"),
	fluidRow(
		id="header",
		column(3,
			id="logoColumn",
			tags$h2("Cancer App",id="logo",title="Super G-Type 2 font by gomarice on 1001fonts.com")
		),
		column(3,
			textInput("searched","Search Genes...",placeholder="ex. ABI1,A2M,ABCD1")
		),
		column(3,
			tags$br(),
			tags$input(type="file",id="uploader",multiple="false",onchange="onUpload()")
		),
		column(3,
			id="shoutoutColumn",
			tags$br(),
			tags$a("by Alex Lugo",href="http://alugocp.github.io/resume",title="Click to access Alex's resume",style="vertical-align:center")
		)
	),
	fluidRow(
		mainPanel(
			tags$canvas(id="canvas",width=200,height=200,onclick="clickCanvas()",onmousedown="mousedown(event)",onmousemove="mousemove(event)",onmouseleave="selected=null"),
			tags$br(),
			tags$br(),
			tags$br(),
			plotOutput("km"),
			textOutput("nodeData"),
			tags$script(src="frontEnd.js")
		)
	)
))
