library(shiny)
shinyUI(fluidPage(
	tags$title("Kaplan-Meier Estimator"),
	fluidRow(
		column(3,
			tags$h2("Hi"),
			"What's up?",
			br(),
			"Look what I made let's goooooOOOOOooooo",
			br(),
			submitButton("Eyyy a button"),
			tags$input(type="submit",onclick="alert('hi')",value="hey")
		),
		mainPanel(
			checkboxInput("gender","Show by Sex?"),
			plotOutput("selectedGraph")
		)
	),fluidRow(
		column(12,
			"Made by me."
		)
	)
))