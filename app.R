library(shiny)
library(shinyalert)
library(tidyverse)
library(rsconnect)
rsconnect::deployApp('../ST558_Project2')

superstore_data <- read_excel("US_Superstore_data.xls", sheet = "Orders")

category_vars <- c("Segment", "Category", "Sub-Category", "Region")
numeric_vars <- c("Sales", "Profit", "Quantity")


# Define UI for the application
ui <- fluidPage(
  
  titlePanel("U.S. Superstore Data"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Subset and Explore!"),
################################################################################
      # Select the first Categorical variable from the drop down list.
      selectInput("cat1",
        label = "First Categorical Variable:",
        choices = category_vars,
        selected = "Segment"
      ),
      # Select the second Categorical variable from the drop down list.
      selectInput("cat2",
        label = "Second Categorical Variable:",
        choices = category_vars,
        selected = "Category"),
################################################################################
      # Dynamic numeric variable sliders
      h2("Choose some numeric subsets."),

      selectInput("num1", 
        label = "First Numeric Variable:", 
        choices = numeric_vars, 
        selected = "Sales"),
      uiOutput("num1_slider"), # Dynamic slider shows here
      
      selectInput("num2", 
        label = "Second Numeric Variable:", 
        choices = numeric_vars, 
        selected = "Profit"),
      uiOutput("num2_slider"), # Dynamic slider shows here
################################################################################
      # Create an action button
      actionButton("subset_data", "Subset the Data")
      ),

################################################################################
      # MAIN PANEL CODE GOES HERE
    mainPanel(
      h3("MAIN PANEL CODE GOES HERE LATER")
    )
  )
)



################################################################################
# Define server logic required to subset the data
server <- function(input, output, session) {
################################################################################
# This code makes sure the select boxes update so the user can't select the same variable in both.
  # Update the first categorical selections available
  observeEvent(input$cat1, {
    updateSelectizeInput(session, "cat2",
                         choices = setdiff(category_vars, input$cat1),
                         selected = input$cat2)
  })
  
  observeEvent(input$cat2, {
    updateSelectizeInput(session, "cat1",
                         choices = setdiff(category_vars, input$cat2),
                         selected = input$cat1)
  })
################################################################################
# Logic for dynamic numeric sliders
  output$num1_slider <- renderUI({
    sliderInput("num1_range",
                label = paste("Select range for", input$num1, ":"),
                min = min(superstore_data[[input$num1]], na.rm = TRUE),
                max = max(superstore_data[[input$num1]], na.rm = TRUE),
                value = c(min(superstore_data[[input$num1]], na.rm = TRUE),
                          max(superstore_data[[input$num1]], na.rm = TRUE)))
  })
  
  output$num2_slider <- renderUI({
    sliderInput("num2_range",
                label = paste("Select range for", input$num2, ":"),
                min = min(superstore_data[[input$num2]], na.rm = TRUE),
                max = max(superstore_data[[input$num2]], na.rm = TRUE),
                value = c(min(superstore_data[[input$num2]], na.rm = TRUE),
                          max(superstore_data[[input$num2]], na.rm = TRUE)))
  })
  
  
  
  # Create reactiveValues() object on the server side to subset data appropriately.
  subsetted_data <- reactiveValues(
    data = NULL)
}

################################################################################
# Now subset the data when the action button is pressed.
observeEvent(input$subset_data, {
  subsetted_data$data <- superstore_data |>
    filter(
      .data[[input$cat1]] %in% unique(superstore_data[[input$cat1]]),
      .data[[input$cat2]] %in% unique(superstore_data[[input$cat2]]),
      .data[[input$num1]] >= input$num1_range[1],
      .data[[input$num1]] <= input$num1_range[2],
      .data[[input$num2]] >= input$num2_range[1],
      .data[[input$num2]] <= input$num2_range[2]
    )
})



# Run the application 
shinyApp(ui = ui, server = server)
