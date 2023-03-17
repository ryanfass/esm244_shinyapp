library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(tmap)
library(dplyr)
library(janitor)
library(here)

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
                      tabPanel("Project Description",
                               sidebarPanel(
                               tags$video(src="fire_timelapse.MOV",
                                          width= "300px",
                                          type="video/mp4", controls="controls")
                               ),
                               mainPanel(
                                 h2("What is prescribed fire?"),
                                 p("Prescribed fire refers to the controlled application of 
                                   fire by a team of fire experts with the goal to restore health 
                                   to ecosystems that depend on fire."),
                                  strong("Watch the Video to see Prescribed Fire in action"),
                                 p("There is a critical need for increased efforts in fire management 
                                   via prescribed fire, but the logistical infeasibility of widespread 
                                   fire application during short ‘burn windows’ of favorable ecological, 
                                   weather and fuels conditions drastically limits implementation"),
                                 h3("The Experiment"),
                                 p("Though the need for more prescribed fire management is 
                                   urgent, land managers are limited by time of year and fuel buildup, 
                                   but also by the phenology of species of concern. Due to drought and 
                                   golden spotted oak borer (GSOB) presence, the Cleveland National Forests burn window is restricted
                                   to periods of black oak (Quercus kelloggii) dormancy. However, this 
                                   restriction was put in place based on the precautionary principle 
                                   (not wanting to do harm to oaks in the absence of data). Our experiment set 
                                   out to test whether extending the Forest Service burn season 
                                   beyond black oak leaf-out in the spring would result in increased black 
                                   oak mortality or negatively affect black oak regeneration. We conducted 
                                   a controlled burn experiment, with prescribed burns applied to experimental 
                                   plots either prior to oak budburst or 3-6 weeks following bud burst. We tracked the initial 
                                   burn severity of mature and sapling oaks, as well as survival and 
                                   resprouting for multiple years after the burn. In addition, we examine 
                                   black oak physiology through xylem pressure potentials to understand sub-lethal
                                   physiological stress that could result in eventual lagged mortality. "),
                                 h3("The Tabs"),
                                 p("Click on the tabs above to explore the affects of prescribed fire timing on black oak 
                                   survivorship, fuel-loading, and black oak physiology.")
                                 )
                               ),
                      
                      tabPanel("Study Site",
                               sidebarPanel(
                                 tmapOutput("old_map")
                               ),
                               mainPanel(
                                 h2("Cleveland National Forest- Mount Laguna"),
                                 p("In the Cleveland National Forest (CNF) land managers seek to extend 
                                   their prescribed burn window in order to get more prescribed fire on 
                                   the ground. "),
                                
                                 p("Mount Laguna sits at about 5,700 feet elevation and is a mixed-conifer, hardwood forest. 
                                   Composed of Jeffery Pine, Black Oak, and Coast-live Oak."),
                                 h3("Map Description"),
                                 p("This map shows our experimental design and plots with tree cover percent after
                                    the experimental burns. The dark green is the 
                                   experimental 'burn window' of burning during black oak leaf-out and the light green
                                   are the areas at which the forest service burned during the current 
                                   allowed 'burned window' durning black oak dormancy."),
                                 p("The dots represent 
                                   our plots and as you hover your mouse over the plot you will see the 
                                   unique Plot Id. Plots outside the polygons are the experiment controls and 
                                   did not experience any fire" )
                                 
                               )
                      ),
                       tabPanel("Trees",
                               sidebarLayout(
                                 sidebarPanel(radioButtons(inputId = 'life_stage',
                                                           label = "Choose Black Oak Life-stage", 
                                                           choices = c("Adult"= "adult","Sapling"= "sapling"))),
                                 mainPanel(
                                   plotOutput("distPlot"),
                                   h3("Tree Status"),
                                   p("Here you can look at how prescribed fire at different times of the burn season
                                     effected black oak survivorship at two different life stages."),
                                   p("Top-Kill: Black oaks have evovled adaptations to fire. Top-kill represents oaks
                                     that saw hight severity fire (lost all their canopy biomass), but were able to persist
                                     by basal resprouting at the base.")
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
                                                 # h3("Xylem Pressure Potentials (XPP)"),
                                                 # p("Predawn xylem pressure potentials are a measure of the water 
                                                 #   potential in the xylem tissue of plants. And are collected inbetween
                                                 #   2am and 5am, when plants are most dormant. Low predawn xylem 
                                                 #   pressure potentials (more negative) indicate that the plant is 
                                                 #   experiencing water stress"),
                                                 # p("We collected XPP as a metric to understand more long-term affects of 
                                                 #   prescribed fire on oaks. For example, higher water stress after fire 
                                                 #   may indicate a potential for lagged mortality.")
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
      theme(axis.text.x = element_text(size =15, 
                                       angle = 25, 
                                       hjust =1),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 20),
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15), 
            strip.text = element_text(size=20))+
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
      theme(axis.text.x = element_text(size =18, 
            angle = 25, hjust =1), 
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 20),
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15), 
            strip.text = element_text(size=20)) #puts a tilt on the x-axis labels
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
