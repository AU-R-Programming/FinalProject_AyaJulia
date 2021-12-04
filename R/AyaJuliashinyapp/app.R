#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library()

# Define UI for application that draws a histogram
ui <- fluidPage(
           # Application title
           headerPanel("General linear model"),
           sidebarPanel(
             p("Select the inputs for the Dependent Variable"),
             selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("AvgIR", "YYYYMM", "SumCount", "AvgLTV", "AvgGFEE", "AvgRTC", "Date")),
             p("Select the inputs for the Independent Variable"),
             selectInput(inputId = "IndVar", label = "Independent Variables", multiple = FALSE, choices = list( "SumCount", "AvgIR", "YYYYMM", "AvgLTV", "AvgGFEE", "AvgRTC", "Date"))
           ),
           mainPanel(
             verbatimTextOutput(outputId = "RegSum"),
             verbatimTextOutput(outputId = "IndPrint"),
             verbatimTextOutput(outputId = "DepPrint")
             #plotOutput("hist")
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
