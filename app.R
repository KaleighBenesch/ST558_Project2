library(shiny)
library(shinyalert)
library(tidyverse)
library(readxl)
library(rsconnect)

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
      tagList(
        selectInput("cat_var", "Select categorical variable:",
                    choices = category_vars),
        selectInput("cat_var2", "Optional 2nd categorical variable:",
                    choices = c(None = ".", category_vars))
      )
    } else {
      tagList(
        selectInput("num_var", "Select numeric variable:",
                    choices = numeric_vars),
        selectInput("num_var2", "Optional 2nd numeric variable (plot tab only):",
                    choices = c(None = ".", numeric_vars)),
        selectInput("cat_var", "Optional categorical variable:",
                    choices = c(None = ".", category_vars))
      )
    }
  })
  
################################################################################
# Exploration tables and summaries (categorical and numeric).
  output$exploration_table <- renderTable({
    req(subsetted_data$data)
    
# CATEGORICAL TABLES
    if (input$explore_type == "cat") {
      req(input$cat_var)
      
      # If the optional second variable is selected to NONE, show a one-way contingency table.
      if (input$cat_var2 %in% c(".", "None")) {
        tbl <- table(subsetted_data$data[[input$cat_var]])
        df <- data.frame(Category = names(tbl), Frequency = as.vector(tbl))
        return(df)
        
      } else {
        # Otherwise, show the two-way contingency table with both categorical variables.
        subsetted_data$data %>%
          count(.data[[input$cat_var]], .data[[input$cat_var2]]) %>%
          pivot_wider(
            names_from = input$cat_var2,
            values_from = n,
            values_fill = 0
          ) 
      }
      
# NUMERIC SUMMARY TABLES
    # Show numeric summary table for only one numeric variable selected.
    } else if (input$explore_type == "num") {
      req(input$num_var)
      if (is.null(input$cat_var) || input$cat_var == ".") {
        subsetted_data$data |>
          summarise(
            mean = mean(.data[[input$num_var]], na.rm = TRUE),
            median = median(.data[[input$num_var]], na.rm = TRUE),
            sd = sd(.data[[input$num_var]], na.rm = TRUE)
          )
      } else { # Show numeric summary table with categorical grouping, if selected.
        subsetted_data$data |>
          group_by(.data[[input$cat_var]]) |>
          summarise(
            mean = mean(.data[[input$num_var]], na.rm = TRUE),
            median = median(.data[[input$num_var]], na.rm = TRUE),
            sd = sd(.data[[input$num_var]], na.rm = TRUE)
          )
      }
    }
  })
################################################################################
# Exploration plots (categorical and numeric).
  
  output$exploration_plot <- renderPlot({
    req(subsetted_data$data)
# Add a loading message while rendering the plot.
    withProgress(message = "Rendering plot...", value = 0, {
      
# CATEGORICAL PLOTS
      if (input$explore_type == "cat") {
        req(input$cat_var)
        
        # If only one categorical variable selected, show basic bar plot.
        if (input$cat_var2 %in% c(".", "None")) {
        ggplot(subsetted_data$data, aes_string(x = paste0("`", input$cat_var, "`"), # Back ticks fix error with "Sub-Category" hyphen in name.
                                               fill = paste0("`", input$cat_var, "`"))) +
          geom_bar() +
          labs(title = paste("Number of Orders by", input$cat_var),
               x = input$cat_var,
               y = "Number of Orders")
          
        } else { # A second optional categorical variable is selected.
          ggplot(subsetted_data$data, aes_string(x = paste0("`", input$cat_var, "`"),
                                                 fill = paste0("`", input$cat_var2, "`"))) +
            geom_bar(position = "dodge") + # Stacked could get too confusing if the user chooses a categorical variable with more than 3 fields.
            labs(title = paste("Number of Orders by", input$cat_var, "and", input$cat_var2),
                 x = input$cat_var,
                 y = "Number of Orders",
                 fill = input$cat_var2)}
        
# NUMERIC PLOTS
      } else {
        req(input$num_var)
        
        # Scatter plot if only one numeric variable is selected.
        if (is.null(input$num_var2) || input$num_var2 %in% c(".", "None")) {
          if (is.null(input$cat_var) || input$cat_var %in% c(".", "None")) {
            # Plot visual
            ggplot(subsetted_data$data, aes_string(x = input$num_var)) +
              geom_histogram(fill = "darkcyan", color = "black", bins = 40) +
              labs(title = paste("Distribution of", input$num_var),
                   x = input$num_var, 
                   y = "Count")
            
        } else { # Create a box plot if one numeric and one categorical variables are selected.
          ggplot(subsetted_data$data, aes_string(x = paste0("`", input$cat_var, "`"), # Back ticks fix error with "Sub-Category" hyphen in name.
                                                 y = paste0("`", input$num_var, "`"), 
                                                 fill = paste0("`", input$cat_var, "`"))) +
            geom_boxplot() +
            coord_flip() +
            labs(title = paste(input$num_var, "by", input$cat_var),
                 x = input$cat_var, 
                 y = input$num_var) +
            theme(legend.position = "none")
        }
          
      } else { # Two numeric variables are selected, no categorical variables are selected.
        if (is.null(input$cat_var) || input$cat_var %in% c(".", "None")) {
          # Plot visual
          ggplot(subsetted_data$data, aes_string(x = input$num_var, 
                                                 y = input$num_var2)) +
            geom_point(color = "darkcyan", alpha = 0.5) +
            labs(title = paste(input$num_var, "vs", input$num_var2),
                 x = input$num_var, 
                 y = input$num_var2)
        } else {
          # Plot visual faceted by one categorical variable.
          ggplot(subsetted_data$data, aes_string(
            x = input$num_var,
            y = input$num_var2)) +
            geom_point(alpha = 0.5, color = "darkcyan") +
            facet_wrap(as.formula(paste("~", paste0("`", input$cat_var, "`")))) +
            labs(title = paste(input$num_var, "vs", input$num_var2, "by", input$cat_var),
                 x = input$num_var, 
                 y = input$num_var2)
          }
        }
      }
    })
  })
}

################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
  
