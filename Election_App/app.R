library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinyWidgets)
election_data <- read.csv("2016_1984.csv", check.names=FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Presidential Partisan Change"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           # tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}")
            sliderInput("slider",
                        "Presidential Election Year",
                        min = 1984,
                        max = 2016,
                        value = 1984,
                        step = 4,
                        sep = '',
                        ticks = TRUE)
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
        winner <- subset(election_data, select = c(paste(input$slider,'W',sep = ''),paste(input$slider,'A+',sep = '')))
        winner[winner[paste(input$slider,'W',sep = '')] == "D", 
               paste(input$slider,'A+',sep = '')] <- winner[paste(input$slider,'A+',sep = '')]*-1
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$polarization <- DT::renderDataTable(
    DT::datatable(data = election_data[c('All Counties', 'StateName', input$slider,
                                       paste(input$slider,'D',sep = ''),
                                       paste(input$slider,'R',sep = ''),
                                       paste(input$slider,'O',sep = ''),
                                       paste(input$slider,'W',sep = '')
                                       )], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
