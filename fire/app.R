library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(tmap)
# source("/helpers.R")
trees_old <- read_csv("data/Laguna_TreesRaw_Master/plot_lifestage_shiny.csv")
fuels_old <- read_csv("data/fuels/fuels_old_summary.csv")
xpp_old <- read_csv("data/xpp_old_group_reorder.csv")
old_map <- read_sf(here('fire/data/map_old/July2020.shp')) %>% 
  clean_names() %>% 
  mutate(burn= recode(burn_type, 'Postbud break' ="leafout", 'Dormant'="dormant"))

cnf_map <- read_sf(here('fire/data/map_old/Ecology_Plots_2021_ALL.shp')) %>% 
  clean_names() 

cnf_plots <- cnf_map %>% 
  filter(site_year %in% c( "Laguna_2021"),
         treatment != "Unit13") 


cnf_plots_26911_sf <- st_transform(cnf_plots, 26911)



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
                      tabPanel("What is Prescribed Fire?",
                               sidebarPanel(
                               tags$video(src="fire_timelapse.MOV",
                                          width= "300px",
                                          type="video/mp4", controls="controls")
                               ),
                               mainPanel(
                                 h2("Project Description"),
                                 p("Prescribed fire refers to the controlled application of 
                                   fire by a team of fire experts with the goal to restore health 
                                   to ecosystems that depend on fire."),
                                 p("There is a critical need for increased efforts in fire management 
                                   via prescribed fire, but the logistical infeasibility of widespread 
                                   fire application during short ‘burn windows’ of favorable ecological, 
                                   weather and fuels conditions drastically limits implementation"),
                                 p("Our experiment set out to test whether extending their burn season 
                                   beyond black oak leaf-out in the spring would result in increased black 
                                   oak mortality or negatively affect black oak regeneration.")
                                 )
                               ),
                      
                      tabPanel("Study Site",
                               mainPanel(
                                 tmapOutput("old_map")
                               )
                      ),
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
                                                                 selected = "ton_acre_1hr"),
                                                                 
                                            ),
                                 mainPanel = (
                                   plotOutput("fuelPlot")
                                 )
                               )),
                     
                       tabPanel("Xylem Pressure Potential",
                                sidebarLayout(
                                  sidebarPanel(selectInput(inputId = "adult_sapling", 
                                                           label = h3("Select Black Oak Life-stage"), 
                                                           choices = list("Adult" = "Adult", "Sapling" = "Sapling"), 
                                                           selected = "Adult")),
                                               mainPanel=(
                                                 plotOutput("xppPlot")
                                               )
                                  )
                                  ),
                                
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
        theme(axis.text.x = element_text(size =12, angle = 25, hjust =1), axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text = element_text(size = 15), legend.title = element_text(size = 15), strip.text = element_text(size=20))
      
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
      theme(axis.text.x = element_text(size =15, angle = 25, hjust =1),axis.title = element_text(size = 15),plot.title = element_text(size = 20),legend.text = element_text(size = 15), legend.title = element_text(size = 15), strip.text = element_text(size=20))+
      ggtitle("Fuels Tons Per Acre")
    
  })
  
####Study Site Map
  output$old_map <- renderTmap({
    tm_shape(old_map) + #Look up vinette on tmap online
      tm_fill("burn", palette = 'BuGn') +
      tm_shape(cnf_plots_26911_sf) +
      tm_dots( id = "plot_id",  "tree_cover")
  })
  
####xpp plot
  output$xppPlot <- renderPlot({
     xpp_old %>% 
       mutate(monitoring_status=fct_relevel(monitoring_status, "preburn", "postburn_year1")) %>%
       arrange(monitoring_status) %>% 
       filter(adult_sapling == input$adult_sapling) %>%
    ggplot( aes( x = treatment, y = average))+
      geom_bar(stat= "identity",
               # color="black",
               fill= "lightblue")+
      facet_wrap(~monitoring_status)+
      ylab('Number of Trees') + 
      ggtitle("Old Experiment Adults- xylem pressure potentials")+
      xlab('Monitoring Status')+
      facet_wrap(~monitoring_status)+
      theme_classic()+
      theme(axis.text.x = element_text(size =18, angle = 25, hjust =1), axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text = element_text(size = 15), legend.title = element_text(size = 15), strip.text = element_text(size=20)) #puts a tilt on the x-axis labels
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
