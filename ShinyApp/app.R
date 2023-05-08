library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Funding Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("curr_discount_rate", "Current Discount Rate:", 
                        min = 0.05, max = 0.08, step = 0.001, value = 0.06),
      selectInput("roa_scenario", "ROA Scenario:", 
                  choices = c("Assumption", "6% Constant", "Recession"), 
                  selected = "Assumption")
    ),
    mainPanel(
      plotOutput("fund_ratio_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Generate fund ratio plot
  output$fund_ratio_plot <- renderPlot({
    # Call get_funding_data() with user-selected inputs
    fund_data <- get_funding_data(curr_disc_rate = as.numeric(input$curr_discount_rate),
                                  new_disc_rate = new_discount_rate_,
                                  cola = 0.03,
                                  retire_refund_ratio = 0.6,
                                  funding_policy = 'ADC',
                                  analysis_type = 'Deterministic',
                                  roa_scenario = input$roa_scenario)
    
    print(fund_data$funded_ratio_mva)
    
    # Create line plot of fund ratio with custom xlim and ylim values
    plot(fund_data$Year, fund_data$funded_ratio_mva, type = "l", 
         xlab = "Year", ylab = "Fund Ratio", 
         main = "Fund Ratio by Year", 
         xlim = c(2021, 2052), ylim = c(0, 1.2))
  })
}

# Run app
shinyApp(ui = ui, server = server)