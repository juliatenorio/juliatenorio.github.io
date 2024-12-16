#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)
historical_spending <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Valentine's Day Interactive Map"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_slider_ui"),
      selectInput(
        inputId = "y_axis_variable",
        label = "Select a Variable to Plot (Money Spent on): ", 
        choices = c("Candy", "Flowers", "Jewelry", "GreetingCards", "EveningOut", "Clothing", "GiftCards"),
        selected = "Candy"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("selected_years"),
      tableOutput("filtered_data"),
      plotOutput("valentines_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$year_slider_ui <- renderUI({
    req(historical_spending)
    req(historical_spending$Year)
    
    sliderInput(
      "year_slider", 
      "Select Year Range:", 
      min = min(historical_spending$Year, na.rm = TRUE),
      max = max(historical_spending$Year, na.rm = TRUE),
      value = c(min(historical_spending$Year, na.rm = TRUE), max(historical_spending$Year, na.rm = TRUE)),
      step = 1, sep = "")
    
  })
  
  filtered_data <- reactive({
    req(input$year_slider)
    historical_spending |> 
      filter(Year >= input$year_slider[1], Year <= input$year_slider[2])
  })
  output$selected_years <- renderText({
    req(input$year_slider)
    paste("Selected Year Range:", input$year_slider[1], "to", input$year_slider[2])
  })
  
  # Show the filtered dataset
  output$valentines_plot <- renderPlot({
    req(filtered_data())
    req(input$y_axis_variable)
    
    ggplot(filtered_data(), aes(x = Year, y = .data[[input$y_axis_variable]])) +
      geom_line(color = "pink") +
      geom_point(color = "darkgreen") +
      labs(
        title = paste("Year vs Money Spent on ", input$y_axis_variable),
        x = "Year",
        y = input$y_axis_variable
      ) +
      theme_minimal()
    
  })
  
  output$filtered_data <- renderTable({
    filtered_data()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)