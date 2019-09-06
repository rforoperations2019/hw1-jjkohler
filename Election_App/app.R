library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
election_data <- read.csv("2016_1984.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Presidential Partisan Change"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           # tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}")
            sliderInput("bins",
                        "Number of bins:",
                        min = 1984,
                        max = 2016,
                        value = 1984,
                        step = 4,
                        sep = '',
                        ticks = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           # Show data table ---------------------------------------------
           DT::dataTableOutput(outputId = "polarization")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$polarization <- DT::renderDataTable(
    DT::datatable(data = election_data[, 1:7], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
