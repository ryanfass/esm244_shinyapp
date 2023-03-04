library(tidyverse)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("Prescribed Fire Timing On Mt. Laguna"),

        # Tabs of our different widgets
          tabsetPanel(type = "tabs",
                      tabPanel("Prescribed Fire", tags$video(src="fire_timelapse.MOV",width= "300px", type="video/mp4", controls="controls")),
                      tabPanel("Study Site"),
                      tabPanel("Trees",
                               sidebarLayout(
                                 sidebarPanel(radioButtons(inputId = 'penguin_species',
                                                           label = "Choose Black Oak Life-stage",
                                                           choices = c("Adult", "Sapling", "Seedling"))),
                                 mainPanel(
                                   htmlOutput("distPlot")
                                 )
                               )
                               ),
                      tabPanel("Fuel Loading"),
                      tabPanel("Fire Behavior")
                      )
) ### end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
