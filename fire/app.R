library(tidyverse)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("Prescribed Fire Timing On Mt. Laguna"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel("Put my widgets here!",
                   radioButtons(inputId = 'penguin_species',
                                label = "Choose Black Oak Life-Stage",
                                choices = c("Adult", "Sapling", "Seedling")),
                   "Choose a color",
                   selectInput(inputId = 'pt_color',
                               label = 'choose your favorite color!',
                               choices = c('Awesome red!' = 'red',
                                           'Pretty purple' = 'purple',
                                           'OOOOORange' = 'orange')),
                   selectInput("select", label = h3("Fuel Loading"), 
                               choices = list("1 Hr Fuels" = 1, "10 Hr Fuels" = 2, "100 Hr Fuels" = 3, "1000 Hr Fuels" = 4, "Duff Height" = 5, "Litter Height" = 6, "Total Fuels" = 7), 
                               selected = 1),
                   
                   hr(),
                   fluidRow(column(3, verbatimTextOutput("value"))),
                   selectInput("select", label = h3("Fire Behavior"), 
                               choices = list("Char Height" = 1, "Scorch Height" = 2, "Scorch Percent" = 3, "Torch Percent" = 4), 
                               selected = 1),
                   
                   hr(),
                   fluidRow(column(3, verbatimTextOutput("value")))
      ), ### End Sidebar Panel

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        ) ### End Main Panel
    ) ### End sidebar Layout
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
