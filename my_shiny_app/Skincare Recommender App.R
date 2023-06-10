skincare_data <- read.csv("ShinyApp_data.csv")

library(shiny)
library(dplyr)
library(DT)
library(shinyalert)

# Define the UI
ui <- fluidPage(
  useShinyalert(),  # Add shinyalert
  
  # Add CSS styling
  tags$head(
    tags$style(
      HTML(
        "
    body {
      background-color: #F5DEB3;  /* Set background color */
      margin: 0;  /* Reset default margins */
      padding: 0; /* Reset default padding */
      font-size: 16px; /* Set default font size */
      font-family: 'Helvetica', sans-serif; /* Set custom font */
    }
    h1 {
      color: #333333;  /* Set font color for title */
      font-family: 'Cookie', cursive;  /* Set font family for title */
      margin-top: 30px; /* Add top margin */
    }
    .selectize-input {
      background-color: #ffffff;  /* Set background color for input widgets */
    }
    .container {
      max-width: 960px; /* Set maximum width for content */
      margin: 0 auto; /* Center align content */
      padding: 20px; /* Add padding to content */
    }
    .sidebar {
      background-color: #EDDDA0; /* Set background color for sidebar */
      padding: 20px; /* Add padding to sidebar */
    }
    "
      )
    )
  ),
  
  titlePanel(
    tags$h1("Skincare Product Recommender")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Instruction section
      tags$div(
        class = "sidebar",
        tags$h2("Instructions"),
        tags$p("Follow these steps to use the Skincare Product Recommender:"),
        tags$ol(
          tags$li("Select the desired brand from the 'Select your Brand' dropdown. You can choose 'All' to include all brands."),
          tags$li("Choose the product type from the 'Select product type' dropdown. You can choose 'All' to include all types."),
          tags$li("Adjust the price range using the 'Select price range' slider. This will filter the products within the selected price range. Please be aware that the price displayed is not in RM."),
          tags$li("The recommended products will be displayed in the 'Recommended Products' tab based on your preferences.")
        )
      ),
      
      # Add input widgets for user preferences
      selectInput("Brand", "Select your Brand:",
                  choices = c("All", unique(skincare_data$Brand))),
      selectInput("product_Type", "Select product type:",
                  choices = c("All", unique(skincare_data$Type))),
      sliderInput("Price", "Select price range:",
                  min = 0, max = 300, value = c(0, 300))
    ),
    mainPanel(
      # Add tabsetPanel for multiple tabs
      tabsetPanel(
        # Recommended products tab
        tabPanel("Recommended Products",
                 class = "container",
                 dataTableOutput("recommended_products")
        ),
        
        # Ingredients tab
        tabPanel("Ingredients",
                 class = "container",
                 uiOutput("ingredients_output")
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Create a reactive expression for filtering the products
  filtered_products <- reactive({
    products <- skincare_data
    
    # Apply filters based on user preferences
    if (input$Brand != "All") {
      products <- products %>% filter(Brand == input$Brand)
    }
    if (input$product_Type != "All") {
      products <- products %>% filter(Type == input$product_Type)
    }
    products <- products %>% filter(Price >= input$Price[1], Price <= input$Price[2])
    
    # Arrange the products in the desired order
    products <- products %>% arrange(Brand, Type, Price)  # Modify the order as needed
    
    return(products)
  })
  
  # Show the welcome popup on app launch
  shinyalert(
    session = session,
    title = "Welcome to Skincare Product Recommender",
    text = "Follow the instructions on the sidebar to find the perfect skincare products for you!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE
  )
  
  # Render the table of recommended products
  output$recommended_products <- renderDataTable({
    datatable(
      filtered_products() %>% select(-Ingredients),
      options = list(
        columnDefs = list(
          list(
            targets = "Price",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '&#163;' + data.toFixed(2);",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      )
    )
  })
  
  # Render the Ingredients output below the recommended products
  output$ingredients_output <- renderUI({
    ingredients <- filtered_products()$Ingredients
    if (length(ingredients) > 0) {
      tags$div(
        tags$h3("Ingredients:"),
        tags$ul(
          lapply(ingredients, function(ingredient) {
            tags$li(ingredient)
          })
        )
      )
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
