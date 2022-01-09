
library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)

euro5 = read.csv("Euro5_6Yr_Data.csv")

# UI
ui <- fluidPage(
  
  # Title
  titlePanel("European Top 5 Leagues - Team Shots per Season from 2015/16-2020/21"),
  
  # Sidebar
  #sidebarLayout(
  sidebarPanel(
    sliderInput("bins", "Number of bins:",
                min = 1,
                max = 50,
                value = 10)
    ),
  # Main Panel
  mainPanel(
    plotOutput("distPlot"),
    textOutput("usefullabel"),
    verbatimTextOutput("summaryofvariable"),
    plotOutput("scatter")
    )
  )

# Server
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Bin reference
    x <- euro5[, 5] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Histogram
    hist(x, breaks = bins, col = 'blue', border = 'white', 
         xlab = "Total Shots per Season", main = "Histogram of Total Shots by Team per Season")
    })
  
  output$summaryofvariable <- renderPrint({
    x <- euro5[, 5]
    summary(x)
    })
  
  output$usefullabel <- renderText({
    "Summary of Total Shots by Team per Season"
    })
  
  output$scatter <- renderPlot({
    y = euro5[, 4]
    x = euro5[, 5]
    plot(y~x, xlab = "Total Shots", ylab = "Points", main = "Points vs Total Shots")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

