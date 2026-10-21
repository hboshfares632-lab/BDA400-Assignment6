library(shiny)
library(ggplot2)
library(quantmod)

# 1. Fetch historical stock data
stock_symbol <- "AAPL" 
start_date <- "2023-01-01" 
end_date <- "2023-07-01" 
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# 2. Define UI component
ui <- fluidPage(
  titlePanel("Interactive Portfolio Dashboard - Technical Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:", start = "2023-01-01", end = "2023-07-01"),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      checkboxGroupInput("technical_indicators", "Select Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"))
    ),
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)

# 3. Define Server component
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    filtered_data <- subset(stock_data, index(stock_data) >= input$date_range[1] & 
                              index(stock_data) <= input$date_range[2])
    
    close_prices <- Cl(filtered_data)
    p <- ggplot(data = filtered_data, aes(x = index(filtered_data), y = as.numeric(close_prices))) +
      geom_line(color = "black", linewidth = 1) +
      labs(title = "Stock Price Analysis", x = "Date", y = "Closing Price ($)") +
      theme_minimal()
    
    print(p)
  })
}

# 4. Run the Shiny App
shinyApp(ui, server)