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

    data <- reactive({subset(election_data, select = c('All Counties', 
                                                       'StateName', 
                                                       input$slider,
                                                       paste(input$slider,'D',sep = ''),
                                                       paste(input$slider,'R',sep = ''),
                                                       paste(input$slider,'O',sep = ''),
                                                       paste(input$slider,'W',sep = ''),
                                                       paste(input$slider,'A+',sep = '')
                                                       )
                             )
        }
        )
    winner <- reactive({data() %>% mutate(.[[8]] = ifelse(.[[7]] == "D", .[[8]]*-1, .[[8]]))})
    
    # winner <- reactive({within(data()[.[,7] == "D", .[,8]] <- data()[,8]*-1)})
    
    # within(df, Name[Name == 'John Smith' & State == 'WI'] <- 'John Smith1')

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R


        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw a histogram of partisan divide paterns
        h <- hist(winner()[,2], breaks = 75, border = 'white')
        cuts <- cut(h$breaks, c(-50,-2,2,50))
        plot(h, ylim = c(0,250), xlim = c(-60,60), col=c("dodgerblue2","mediumslateblue","brown2")[cuts])
    })
    
    output$polarization <- DT::renderDataTable(
    DT::datatable(data = data()[,1:7], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
