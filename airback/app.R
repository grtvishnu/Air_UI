library(shiny)
library(shinydashboard)    
library(tidyverse)
library(nortest)
library(mvnormtest)
library(MASS)
library(shinyLP)
library(class)
library(gmodels)
library(caret)
library(rattle)
library(ranger)
library(klaR)
library(kernlab)
library(micad)
library(e1071)
library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(mclust)
library(keras)
library(mlbench) 
library(randomForest)
library(Metrics)
library(BBmisc)
library(corrplot)
library(lars)
library(xgboost)
library(Matrix)
library(methods)
library(mlr)
library(data.table)
library(lubridate)
library(scales)
library(dashboardthemes)
library(shinythemes)
library(plotly)
library(flexdashboard)
ui <- dashboardPage(
        dashboardHeader(title = "Air pollution"),
        dashboardSidebar(
            sidebarMenu("dataset", tabName ="Dataset", icon("dashboard"))) ,
        
        dashboardBody(
            tabItems(
                tabItem("Dataset",
                        fluidPage(
                        h1("Data")))
            )
        )
)
#Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

               