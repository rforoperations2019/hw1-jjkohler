library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(plotly)
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
                        ticks = TRUE),
            
            # checkboxGroupButtons(
            #     inputId = "somevalue", label = "Data Aggregation Level :", 
            #     choices = c("Choice A", "Choice B"), 
            #     justified = TRUE, status = "primary",
            #     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                
            radioButtons(inputId="choice", label="Aggregation Level :",
                             choices=c("State","County"))
            # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           plotOutput("OtherVoting"),
           
           plotOutput("votePie"),
           
           # Show data table ---------------------------------------------
           DT::dataTableOutput(outputId = "polarization")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactive({sub <- subset(election_data, select = c('All Counties', 
                                                       'StateName', 
                                                       input$slider,
                                                       paste(input$slider,'D',sep = ''),
                                                       paste(input$slider,'R',sep = ''),
                                                       paste(input$slider,'O',sep = ''),
                                                       paste(input$slider,'W',sep = ''),
                                                       paste(input$slider,'W+',sep = ''),
                                                       paste(input$slider,'%O',sep = '')
                                                       )
                                    
                             ) 
    colnames(sub) <- c('County', 'State', 'Total Votes', 'Dem Votes', 'Rep Votes', 'Other Votes', "Win","Lean")
    sub

        }
        )

    winner <- reactive({
        test <- data()[,7:8]
        colnames(test) <- c("party","lean")
        test[test['party'] == 'D', 'lean'] <- test['lean']*-1
        test
        })

    agg <- reactive({aggregate(x = data()[c('Total Votes', 'Dem Votes', 'Rep Votes', 'Other Votes')],
                           by = list(unique.values = data()$State),
                           FUN = sum)})


    output$distPlot <- renderPlot({

        # draw a histogram of partisan divide paterns
        h <- hist(winner()[,2], breaks = 75, border = 'white')
        cuts <- cut(h$breaks, c(-50,-2,2,50))
        plot(h, ylim = c(0,250), xlim = c(-60,60), col=c("dodgerblue2","mediumslateblue","brown2")[cuts],
             main='Number of Counties by Partisan Lean Percentage')
    })
    
    output$OtherVoting <- renderPlot({
        
        # draw a histogram of partisan divide paterns
        h <- hist(data()[,9], breaks = 75, border = 'white')
        cuts <- cut(h$breaks, c(0,.15,.3,.5))
        plot(h, xlim = c(0,.4), ylim = c(0,300), col=c("palegreen2","palegreen3","palegreen4")[cuts])
    })
    
    output$VotePie <- renderPlot({
        p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie') %>%
            layout(title = 'United States Personal Expenditures by Categories in 1960',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
            #     pie <- bp + coord_polar("y", start=0)
    # pie + scale_fill_brewer("Blues") + blank_theme +
    #     theme(axis.text.x=element_blank())+
    #     geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
    #                   label = percent(value/100)), size=5)
    })
        
        
    output$polarization <- DT::renderDataTable(
    DT::datatable(data = data()[,1:7], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
