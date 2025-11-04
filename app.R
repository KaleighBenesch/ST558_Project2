library(shiny)
library(shinyalert)
library(tidyverse)
library(rsconnect)
rsconnect::deployApp('../ST558_Project2')

superstore_data <- read_excel("US_Superstore_data.xls", sheet = "Orders")

category_vars <- c("Segment", "Category", "Sub-Category", "Region")
numeric_vars <- c("Sales", "Profit", "Quantity", "Discount")

################################################################################
# UI
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
      tabsetPanel(
        id = "tabs",
################################################################################
# ABOUT TAB
        tabPanel("About",
                 h2("About the App"),
                 p("Welcome to the Data Exploration App!"),
                 p("This Shiny app allows you to explore sales, profits, and other variables in a real U.S. Superstore dataset. 
                   Use the sidebar to subset the data based on consumer and product details, then explore the data across various settings."),

                # The data and it's source.
                 h3("About the Data"),
                 p("This dataset gives insights on online orders of a superstore in the U.S. 
                   It includes consumer orders from 2014–2018, with details about sales, profit, category, etc."),
                 p(HTML("For further information on the dataset, see here: 
                   <a href='https://www.kaggle.com/datasets/vivek468/superstore-dataset-final' target='_blank'>
                   US Superstore data on Kaggle</a>")),
                  # Image related to the dataset.
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d8/Superstore_%28Universal_Television_sitcom%29.svg/2560px-Superstore_%28Universal_Television_sitcom%29.svg.png", 
                     height = "80px"),

                 h3("Using the App"),
                 tags$ul(
                   tags$li("Use the sidebar to subset the dataset."),
                   tags$li("Go to the 'Data Download' tab to see or download your subsetted data."),
                   tags$li("Explore the data for summaries and visualizations.")
                 )
        ),
################################################################################
# DATA DOWNLOAD TAB
        tabPanel("Data Download",
                 h2("See and Download Your Subsetted Data"),
                 p("After choosing your subsets in the sidebar, click 'Subset the Data' button to update this table."),
                 DT::dataTableOutput("data_table"),
                # Save the user's data as a file.
                 downloadButton("download_data", "Download Your Data")
        ),
################################################################################
# DATA EXPLORATION TAB
        tabPanel("Data Exploration",
                 h2("Obtain Summaries and Visualizations"),
                 
                 # Choose to display categorical data summaries or numeric variable summaries.
                 radioButtons("explore_type", "Choose type of exploration:",
                              choices = c("Categorical" = "cat",
                                          "Numeric" = "num")),
                 uiOutput("explore_inputs"),
                 
                 # Use subtabs for either tables or plots
                 tabsetPanel(
                   tabPanel("Table", tableOutput("exploration_table")),
                   tabPanel("Plot", plotOutput("exploration_plot"))
                 )
        )
      )
    )
  )
)



######################################################################################################################################
# SERVER LOGIC
# Define server logic required to subset the data
server <- function(input, output, session) {
################################################################################
# This code makes sure the select boxes update so the user can't select the same variable in both.
  # Update the CATEGORICAL selections available
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
  
  # Update the NUMERIC selections available
  observeEvent(input$num1, {
    updateSelectizeInput(session, "num2",
                         choices = setdiff(numeric_vars, input$num1),
                         selected = input$num2)
  })
  
  observeEvent(input$num2, {
    updateSelectizeInput(session, "num1",
                         choices = setdiff(numeric_vars, input$num2),
                         selected = input$num1)
  })
################################################################################
# Logic for the two dynamic numeric sliders.
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

################################################################################
# Create reactiveValues() object on the server side to subset data appropriately.
  subsetted_data <- reactiveValues(
    data = NULL)

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
  
# Display the data table when the action button is pressed in side panel.
  output$data_table <- DT::renderDataTable({
    req(subsetted_data$data)
    DT::datatable(subsetted_data$data, options = list(pageLength = 5))
  })
  
################################################################################
# Handling the user's download from DATA DOWNLOAD tab.
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("superstore_subsetted.csv")
    },
    content = function(file) {
      write.csv(subsetted_data$data, file, row.names = FALSE)
    }
  )
  
################################################################################
# Variable selection based on numerical or graphical summary type in DATA EXPLORATION tab.
  output$explore_inputs <- renderUI({
    req(subsetted_data$data) # Check if subsetted data exists.
    
    if (input$explore_type == "cat") {
      selectInput("cat_var", "Select categorical variable:",
                  choices = category_vars)
    } else {
      tagList(
        selectInput("num_var", "Select numeric variable:",
                    choices = numeric_vars),
        selectInput("cat_var", "Optional categorical variable:",
                    choices = c(None = ".", category_vars))
      )
    }
  })
  
################################################################################
# Categorical contingency table.
  output$exploration_table <- renderTable({
    req(subsetted_data$data)
    validate(
      need(input$explore_type == "cat", "Select 'Categorical' to see this table.")
    )
    req(input$cat_var)
    
    table(subsetted_data$data[[input$cat_var]])
  }, rownames = TRUE)
  
# Categorical Bar Plot
  output$exploration_plot <- renderPlot({
    req(subsetted_data$data)
    validate(
      need(input$explore_type == "cat", "Select 'Categorical' to see this plot."),
      need(input$cat_var, "Select a categorical variable to plot.")
    )
    
# Add a loading message while rendering the plot.
    withProgress(message = "Rendering plot...", value = 0, {
# Plot visual
    ggplot(subsetted_data$data, aes_string(x = input$cat_var, fill = input$cat_var)) +
      geom_bar() +
      labs(title = paste("Number of Orders by", input$cat_var),
           x = input$cat_var,
           y = "Number of Orders")
  })
})
}

################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
  
