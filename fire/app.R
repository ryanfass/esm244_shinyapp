library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
# source("/helpers.R")
trees_old <- read_csv("data/Laguna_TreesRaw_Master/plot_lifestage_shiny.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # # Application title
    # titlePanel("Prescribed Fire Timing On Mt. Laguna"),
    # Application title
    titlePanel(title = div(span(img(src = "flame.png",width= "50px",height= "50px")
                                ,
                     "Prescribed Fire Timing on Mt. Laguna"))),

        # Tabs of our different widgets
          tabsetPanel(type = "tabs",
                      tabPanel("What is Prescribed Fire?", tags$video(src="fire_timelapse.MOV",width= "300px", type="video/mp4", controls="controls")),
                      tabPanel("Study Site"),
                       tabPanel("Trees",
                               sidebarLayout(
                                 sidebarPanel(radioButtons(inputId = 'life_stage',
                                                           label = "Choose Black Oak Life-stage",
                                                           choices = c("Adult"= "adult","Sapling"= "sapling"))),
                                 mainPanel(
                                   plotOutput("distPlot")
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
      message("inside distplot, input$life_stage=", input$life_stage)
      trees_old %>% 
        mutate(monitoring_status=fct_relevel(monitoring_status, "PreBurn", "PostBurnYear1", "PostBurnYear2")) %>%
        arrange(monitoring_status) %>% 
        filter(life_stage == input$life_stage) %>% 
      ggplot(aes(fill = status, x = treatment_burn, y = count))+
        geom_bar(stat= "identity", 
                 position= "stack",
                 color="black")+
        scale_fill_manual(values = c('chocolate', 'darkgreen', 'green'))+
        # geom_text(aes(y = 30,label= treatment_burn),
        #           position = position_dodge(width = .9),
        #           angle = 90,
        #          fontface= "bold")+
        facet_wrap(~monitoring_status)+
        ylab('Number of Trees') + 
        xlab('Treatment')+
        ggtitle("Adult Quercus kelloggii- Old Exp.")+
        theme_classic()+
        theme(axis.text.x = element_text(size =9, angle = 25, hjust =1))
      
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
