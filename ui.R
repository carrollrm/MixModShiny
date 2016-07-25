library(shiny)

shinyUI(fluidPage(
	titlePanel("Bayesian spatio-temporal multivariate mixture models"),
sidebarLayout(
	sidebarPanel(
		helpText("Display Output for Spatio-temporal Cancer Incidence Data"),
		selectInput("mod",label="Choose a fitted model",
			choices=list("F1","F2","F3","F4"), selected="F1"),
		radioButtons("data",label="Model Complexity:",c("Univariate","Multivariate")),
		radioButtons("which",label="Type of Output:",c("Summary","Mixture Parameter","Random Effects")),
		submitButton("Submit")
	),
	mainPanel(
	  textOutput("text"),
		plotOutput("map"),
		tableOutput("tab"),
		textOutput("text2"),
		tags$head(tags$style("#text2{color: red;
		                     font-style: italic;
		                     }"))
	)
)
))