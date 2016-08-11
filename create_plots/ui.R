library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region:", unique(plot.data.all$Region)),
      selectInput("period", "Period:", unique(plot.data.all$Period)),
      selectInput("type",   "Type:",   unique(plot.data.all$Type))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")
    )
  )
))

