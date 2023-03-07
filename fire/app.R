library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(tmap)
# source("/helpers.R")
trees_old <- read_csv("data/Laguna_TreesRaw_Master/plot_lifestage_shiny.csv")
fuels_old <- read_csv("data/fuels/fuels_old_summary.csv")

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
                     
                       tabPanel("Fuel Loading",
                               sidebarLayout(
                                 sidebarPanel(checkboxGroupInput(inputId = "fuel_type", 
                                                                 label = h3("Choose Fuel Type (tons/acre)"), 
                                                                 choices = list("1 hr" = "ton_acre_1hr", "10hr" = "ton_acre_10hr", "100hr" = "ton_acre_100hr", "1000hr"= "ton_total_1000hr", "Duff and litter"= "duff_litter_tons", "Total Fuel"= "total_tons_acre"), 
                                                                 selected = 1),
                                                                 
                                            ),
                                 mainPanel = (
                                   plotOutput("fuelPlot")
                                 )
                               )),
                     
                       tabPanel("Fire Behavior")
                      )
) ### end ui fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

  ####Trees output graph
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
        ggtitle("Quercus kelloggii")+
        theme_classic()+
        theme(axis.text.x = element_text(size =9, angle = 25, hjust =1))
      
    })
####Fuels data Reactive
  fuel_reactive <- reactive({
    x <- fuels_old %>% 
      pivot_longer(cols = c(5:19),
                   names_to = 'fuel_type',
                   values_to = 'tons_per_acre') %>%
      mutate(monitoring_status=fct_relevel(monitoring_status, "PreBurn", "PostBurnYear1", "PostBurnYear2")) %>%
      arrange(monitoring_status) %>% 
      filter(fuel_type %in% input$fuel_type) %>% 
      group_by( monitoring_status, treatment_burn, fuel_type) %>% ### Added fuel_type
      summarise(mean_ton_acre = mean(tons_per_acre))
    return(x)
  })      
    
####Fuels output plot
  output$fuelPlot <- renderPlot({
     ggplot(data=fuel_reactive(), aes(x=fuel_type, y=mean_ton_acre, fill= treatment_burn)) +  ### Changed x from treatment_burn to fuel_type
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c('tan', 'brown', 'darkgreen'))+
      facet_wrap(~monitoring_status)+  
      xlab("")+
      theme_classic()+
      theme(axis.text.x = element_text(size =9, angle = 25, hjust =1))+
      ggtitle("Fuels Tons Per Acre")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
