#-----------------------------------------------------#
# Title:  An Exercise in Shiny Applications...        #
# Author: Brandon Monier (brandon.monier@sdstate.edu) #
# Date:   01.02.17                                    #
#-----------------------------------------------------#

#--------------
# The Shiny app
#--------------


library(shiny)

ui <- fluidPage(
  sliderInput(inputId = 'num', label = 'Choose a number', value = 25,
              min = 1, max = 10000),
  plotOutput('hist')
)

server <- function(input, output){
  output$hist <- renderPlot({
    title <- paste(input$num,'random normal values')
    x <- paste('n =', input$num)
    hist(rnorm(input$num), main = title, xlab = x) 
  })
}

shinyApp(ui = ui, server = server)
