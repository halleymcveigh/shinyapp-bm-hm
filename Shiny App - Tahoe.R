# attach packages
library(shiny)
library(tidyverse)
library(here)
library(tmap)
library(sf)
library(sp)

tmap_mode("view")

# Read in data for spatial analysis
benefits_sf <- read_sf(here("data", "benefits_polygons.shp")) %>% 
  clean_names() %>% 
  select(c(14:33,40:41))

impacts_sf <- read_csv(here("data", "adverse_impacts_polygons.shp")) %>% 
  clean_names() %>% 
  select(c())

mgmt_sf <- read_csv(here("data", "mgmt_prioritization_polygons.shp")) %>% 
  clean_names() %>% 
  select(c())








###### Create the user interface
ui <- fluidPage(theme = "tahoe.css",
                titlePanel("Ecosystem Services of Interest in the Tahoe-Central Sierra Region - Blue Forest Conservation"),
                navbarPage(
                  "Explore the Tahoe Basin",
                  
                  # Tab 1: Introduction
                  tabPanel("Overview",
                           mainPanel("This application visualizes survey responses from individuals in the Tahoe-Central Sierra Region. Surveys were conducted as part of a Group Project at the Bren School.",
                                     br(),
                                     br(),
                                     "The map below depicts the study area.",
                                     br(),
                                     plotOutput
                                     ("tahoe_map"),
                                     br(),
                                     br())),
                 
                   # Tab 2: Ecosystem benefits
                  tabPanel("Ecosystem benefits",
                           sidebarLayout(
                             sidebarPanel(
                               "WIDGET 1",
                               checkboxGroupInput(inputId = "ecosystem_service",
                                                  label = "Select ecosystem service:",
                                                  choices = unique(benefits_tidy_sf$ecosystem_benefit)
                                                  ) # end checkboxGroupInput
                             ), #end of sidebarPanel
                             mainPanel(
                               "The Tahoe-Central Sierra region is a diverse landscape that covers approximately 2.4 million acres of land, multiple National Forests, Wildernesses, cities and cities. The region is rich in diversity, but under threat from climate change impacts. This study sought to determine specific stakeholder interest in various ecosystem services to better prioritize how to protect these services under changing climate and future management scenarios...
                               OUTPUT MAP GOES HERE
                               with a description of how participants were asked to identify regions of interest within our study region
                               NEED TO FIGURE OUT HOW TO INSERT MAP AND PLOT SHAPEFILES OF STUDY REGION HERE",
                               plotOutput("eco_ben_reactive_plot")
                             ) #end of mainPanel
                           ) #end of sidebarLayout
                           ),
                  
                  # Tab 3: Impacts/Risks to ecosystem services
                  tabPanel("Impacts/Risks to Ecosystem Services"),
                  
                  # Tab 4: Management priority areas
                  tabPanel("Priority Management Areas")
                  
                  
                ) #end of navbarPage
                ) #end ui


###### Create server function
server <- function(input, output) {

  
# Tab 1: Introduction
  output$tahoe_map <- renderPlot({
    ggmap(tahoe_map) +
      labs(title = "Tahoe-Central Sierra Region") +
      theme_void()
  })
  
# Tab 2: Ecosystem benefits  
  eco_ben_reactive <- reactive({
    benefits_tidy_sf %>% 
      filter(ecosystem_benefit %in% input$ecosystem_service)
  }) #end sw_reactive
  
  output$eco_ben_reactive_plot <- renderPlot(
    ggplot(data = eco_ben_reactive()) +
      geom_sf(aes(geometry = geometry, fill = ecosystem_benefit), color = "darkcyan", alpha = 0.5) +
      theme_minimal())
  
# Tab 3:
  
# Tab 4:  
}



###### Combine into an app:

shinyApp(ui = ui, server = server)