library(shiny)
library(shinyalert)
library(tidyverse)
library(rsconnect)
rsconnect::deployApp('../ST558_Project2')


# Define UI for the application
ui <- fluidPage(
  
  titlePanel("US Superstore Data"))









# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # CODE GOES HERE
}











# Run the application 
shinyApp(ui = ui, server = server)
