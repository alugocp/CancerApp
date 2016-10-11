library(shiny)
shinyUI(fluidPage(
	tags$title("Alex Lugo's Kaplan-Meier Estimator"),
	fluidRow(
		column(3,
			tags$h2("KM Plotter"),
			selectInput("gene","Pick a factor",choices=list("Age"="age","Weight Loss"="wt.loss","Meal Calories"="meal.cal","Gender"="sex")),
			selectInput("gene1","Pick a factor (optional)",choices=list("-Select-"="none","Age"="age","Weight Loss"="wt.loss","Meal Calories"="meal.cal","Gender"="sex")),
			numericInput("quantile","Pick a K value",0.1,step=0.05,min=0.1,max=0.5),
			submitButton("Eyyy a button"),
			tags$input(type="submit",onclick="alert('hi')",value="hey")
		),
		mainPanel(
			plotOutput("km")
		)
	),fluidRow(
		column(12,
			"Made by me."
		)
	)
))