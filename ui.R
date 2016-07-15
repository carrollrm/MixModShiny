library(shiny)

shinyUI(fluidPage(
	titlePanel("ST Multivariate Mixture Models"),
sidebarLayout(
	sidebarPanel(
		helpText("Create Output for Spatio-temporal Cancer Incidence Data"),
		selectInput("mod",label="Choose a fitted model",
			choices=list("F1","F2","F3","F4"), selected="F1"),
		radioButtons("data",label="Model Complexity:",c("Univariate","Multivariate")),
		submitButton("Submit")
	),
	mainPanel(
	  textOutput("text"),
		plotOutput("map"),
		tableOutput("tab")
	)
)
))