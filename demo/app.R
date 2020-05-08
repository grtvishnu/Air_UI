# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
Chennai <- read_csv("c1_cat.csv")
Mumbai <- read_csv("d1_cat.csv")
Hydrabad <- read_csv("h1_cat.csv")
Kolkata <- read_csv("k1_cat.csv")
umbai <- read_csv("m1_cat.csv")    

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Air Quality Forecast"),
                sidebarLayout(
                    sidebarPanel(
                        
                        # Select type of trend to plot
                        # selectInput(inputId = "type", label = strong("Variable"),
                        #             choices = unique(Mumbai$AQI),
                        #             selected = "AQI"),
                        radioButtons("option", "Select Option", choices = c("Chennai", "Delhi", "Hydrabad", "Kolkata", "Mumbai")),
                        
                        # Select date range to be plotted
                        dateRangeInput("date", strong("Date range"), start = "2016-01-01", end = "2020-01-01",
                                       min = "2016-01-01", max = "2020-02-28"),
                        
                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                        
                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.smoother == true",
                                         sliderInput(inputId = "f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),
                    
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "300px"),
                        textOutput(outputId = "desc"),
                        tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                    )
                )
)

# Define server function
server <- function(input, output) {
    
    # Subset data
    selected_trends <- reactive({
        req(Mumbai$dates)
        validate(need(!is.na(Mumbai$dates[1]) & !is.na(Mumbai$dates[2]), "Error: Please provide both a start and an end date."))
        validate(need(Mumbai$dates[1] < Mumbai$dates[2], "Error: Start date should be earlier than end date."))
        Mumbai %>%
            filter(
                type == input$type,
                date > as.POSIXct(input$dates[1]) & date < as.POSIXct(input$dates[2]
                ))
    })
    
    
    # Create scatterplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        color = "#434343"
        par(mar = c(4, 4, 1, 1))
        plot(x = Mumbai$dates, y = Mumbai$AQI, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = Mumbai$dates, y = Mumbai$AQI, f = input$f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })
    
    # Pull in description of trend
    output$desc <- renderText({
        
        paste("The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)