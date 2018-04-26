#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#
# create client connection as shown in
# https://methods.officialstatistics.org/developers/clients/r/
#
# Shiny demo code from ...
# https://methods.officialstatistics.org/developers/tutorials/sample-apps/shiny-app/
#
#api endpoint for this algorithmn from doc page at ..
# https://methods.officialstatistics.org/algorithms/TimeSeries/FourierDetrend

library(shiny)
library(ggplot2)
# library(ggfortify)
library(algorithmia)

Your_UNGP_API_Key = "simMXXXXXXXXXXXXYYYYYYZZZZZ"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("UN GP version of the NY Births with Fourier Detrend Demo - by Nev"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "K strongest frequencies:",
                  min = 1,
                  max = 10,
                  value = 2)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

fourier_detrend <- function(x) {
  client <- getAlgorithmiaClient(Your_UNGP_API_Key, "https://api.methods.officialstatistics.org")
  algo <- client$algo("TimeSeries/FourierDetrend/0.1.0")
  result <- algo$pipe(x)$result
}

# Define server logic required to draw plot
server <- function(input, output) {
  births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
  # Update the plot based on UI slider
  update_plot = reactive({
    data_input <- list(as.list(births), input$bins)
    input <- list(list(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3), 2)
    return(unlist(fourier_detrend(data_input)))
  })
  
  output$distPlot <- renderPlot({
    detrend_data <- update_plot()
    dbl <- ts(detrend_data, frequency=12, start=c(1946,1))
    autoplot(dbl, ts.colour = "#00B8D4")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
