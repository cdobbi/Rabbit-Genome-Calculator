# Load Shiny library for creating the simple web form app
library(shiny)

# Function to demonstrate output to screen and datatypes
# (numeric, character, logical)
# Takes a numeric price and character type, returns a logical check
# and prints message
display_info <- function(price, type) {
  # price: numeric datatype for cost
  # type: character datatype for rabbit type
  is_expensive <- price > 50  # logical datatype: true if price > 50
  message <- paste(
    "Rabbit type:", type,
    "Cost:", price,
    "Expensive?", is_expensive
  )
  print(message)  # Output to screen
  return(is_expensive)
}

# Function to demonstrate loop with lists/arrays and datatypes (list)
# Loops through a list of rabbit features, prints each
process_list <- function(features) {
  # features: list datatype containing character strings
  for (feature in features) {  # Loop over list
    print(paste("Feature:", feature))  # Output to screen
  }
}

# Function to demonstrate dataframes and CSV
# Creates a dataframe, writes to CSV, reads back, and returns total cost
handle_dataframe_csv <- function(types, quantities) {
  # types: character vector; quantities: numeric vector
  df <- data.frame(
    Type = types,
    Quantity = quantities,
    Cost = c(20, 30)
  ) # Dataframe with columns
  write.csv(df, "rabbit_data.csv", row.names = FALSE)  # Write to CSV
  read_df <- read.csv("rabbit_data.csv")  # Read from CSV
  total_cost <- sum(read_df$Cost * read_df$Quantity)  # Calculate total
  print("Dataframe processed and CSV handled.")  # Output to screen
  return(total_cost)
}

# Shiny UI: Simple form with text inputs and button (no styling)
ui <- fluidPage(
  titlePanel("Rabbit Coat Calculator"),  # App title
  textInput("type", "Rabbit Type (e.g., Angora)", "Angora"),  # Input for type
  numericInput("quantity", "Quantity", 1, min = 1),  # Input for quantity
  actionButton("calculate", "Calculate Cost"),  # Button to trigger calculation
  textOutput("result")  # Output display
)

# Shiny server: Handles logic, calls functions to demonstrate requirements
server <- function(input, output) {
observeEvent(input$calculate, {
  # Call functions to demonstrate features
  display_info(25, input$type)  # Output and datatypes
  process_list(list("Soft", "Warm", "Fluffy"))  # Loop and list
  total <- handle_dataframe_csv(
    c(input$type, "Rex"),
    c(input$quantity, 2)
  ) # Dataframe and CSV
  output$result <- renderText({
    paste(
      "Total Cost:",
    total
    )
    # Display result
  })
}

# Run the Shiny app (starts local web server with simple form)
shinyApp(ui = ui, server = server)
