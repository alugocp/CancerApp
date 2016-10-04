library(shiny)
shinyUI(fluidPage(
	tags$title("Testing out Shiny R..."),
	tags$h2("Historical Empires"),
	sidebarPanel(verticalLayout(
		tags$h2("Hi"),
		"What's up?",
		"Look what I made let's goooooOOOOOooooo",
		submitButton("Eyyy a button")
	)),
	mainPanel(
		checkboxInput("show","Show Chart?"),
		plotOutput("empires")
	)
))