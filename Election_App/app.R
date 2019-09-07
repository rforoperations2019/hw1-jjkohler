library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(plotly)
library(shinyWidgets)
library(shinyjs)
election_data <- read.csv("2016_1984.csv", check.names=FALSE)
election_data[is.na(election_data)] <- 0
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
            

            radioButtons(inputId="choice", label="Data Table Aggregation Level :",
                             choices=c("State","County")),
            
            useShinyjs(),
     
            checkboxInput("hide", "Break Down by State :", FALSE),
            
            selectInput(inputId = "state", 
                        label = "State :",
                        choices = list('States' = sort(election_data$StateName)),
                        selected = list('States' = sort(election_data$StateName))[1]
                        ),
            plotlyOutput("votePie"),
            verbatimTextOutput("event")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           plotOutput("OtherVoting"),
           
           
           
           # Show data table ---------------------------------------------
           DT::dataTableOutput(outputId = "polarization")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #https://stackoverflow.com/questions/38777741/observe-event-to-hide-action-button-in-shiny
    observe({ toggle(id="state", condition=!isFALSE(input$hide))})
    

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
    sub[3:6] <- lapply(sub[3:6], as.integer)
    sub[is.na(sub)] <- 0
    sub

        }
        )

    winner <- reactive({
        test <- data()[,7:8]
        colnames(test) <- c("party","lean")
        test[test['party'] == 'D', 'lean'] <- test['lean']*-1
        test
        })
    
    winner_st <- reactive({
        test <- state_sub_cty()[,7:8]
        colnames(test) <- c("party","lean")
        test[test['party'] == 'D', 'lean'] <- test['lean']*-1
        test
    })

    agg <- reactive({aggregate(x = data()[c('Total Votes', 'Dem Votes', 'Rep Votes', 'Other Votes')],
                           by = list('States' = data()$State),
                           FUN = sum)})
    
    state_sub_cty <- reactive({
        req(input$state)
        shrunk <- data()[data()$State == input$state,]
        shrunk
        })
    
    state_sub <- reactive({
        req(input$state)
        small <- agg()[agg()$States == input$state,]
        small
    })
    
    pie_agg <- reactive({first <- 
        t <- as.data.frame(colSums(data()[,c('Dem Votes', 'Rep Votes', 'Other Votes')]))
          colnames(t) <- c('total')
        t

    })


    output$distPlot <- renderPlot({

        # draw a histogram of partisan divide paterns
        if (input$hide == FALSE) {
        h <- hist(winner()[,2], breaks = 75, border = 'white')
        cuts <- cut(h$breaks, c(-50,-2,2,50))
        plot(h, ylim = c(0,250), xlim = c(-60,60), xlab = 'Partisan Win Margin Relative to National Avg.', ylab = "Counties", 
             col=c("dodgerblue2","mediumslateblue","brown2")[cuts],
             main='Number of Counties by Partisan Lean Percentage')}
        
        if (input$hide == TRUE) {
            h <- hist(winner_st()[,2], breaks = 20, border = 'white')
            cuts <- cut(h$breaks, c(-50,-2,2,50))
            plot(h, ylim = c(0,20), xlim = c(-60,60), xlab = 'Partisan Win Margin Relative to National Avg.', 
                 ylab = "Counties", col=c("dodgerblue2","mediumslateblue","brown2")[cuts],
                 main='Number of Counties by Partisan Lean Percentage')}
        
    })
    
    output$OtherVoting <- renderPlot({
        
        # draw a histogram of partisan divide paterns
        
        if (input$hide == FALSE) {
        h <- hist(data()[,9], breaks = 75, border = 'white')
        cuts <- cut(h$breaks, c(0,.15,.3,.5))
        plot(h, xlim = c(0,.4), ylim = c(0,300),xlab = 'Percent Other Party Vote', ylab = "Counties", col=c("palegreen2","palegreen3","palegreen4")[cuts],
        main='Number of Counties by Other Party Vote Share')}
        if (input$hide == TRUE) {
            h <- hist(state_sub_cty()[,9], breaks = 20, border = 'white')
            cuts <- cut(h$breaks, c(0,.15,.3,.5))
            plot(h, xlim = c(0,.4), ylim = c(0,20),xlab = 'Percent Other Party Vote', ylab = "Counties", col=c("palegreen2","palegreen3","palegreen4")[cuts],
                 main='Number of Counties by Other Party Vote Share')}
        
    })
    
    output$votePie <- renderPlotly({
        p <- plot_ly(pie_agg(), values = ~total, type = 'pie') %>%
            layout(title = 'United States Presidential Vote Breakdown',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        p
        
            #     pie <- bp + coord_polar("y", start=0)
    # pie + scale_fill_brewer("Blues") + blank_theme +
    #     theme(axis.text.x=element_blank())+
    #     geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
    #                   label = percent(value/100)), size=5)
    })
        
        
    output$polarization <- DT::renderDataTable(
        
        if((input$choice == 'County')&(input$hide == FALSE)){
    DT::datatable(data = data()[,1:7], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)}
        
         else if((input$choice == 'County') & (input$hide == TRUE)){
        DT::datatable(data = state_sub_cty()[,1:7], 
                      options = list(pageLength = 10), 
                      rownames = FALSE)}
       
        else if((input$choice == 'State') & (input$hide == TRUE)){
        DT::datatable(data = state_sub()[,1:5], 
                      options = list(pageLength = 10), 
                      rownames = FALSE)}
        else{
        DT::datatable(data = agg()[,1:5], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
