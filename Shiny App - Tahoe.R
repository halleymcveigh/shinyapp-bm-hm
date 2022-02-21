# attach packages
library(shiny)
library(tidyverse)
library(here)
library(tmap)
library(sf)
library(janitor)
library(sp)
library(jpeg)

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
                           mainPanel(h3("Study Overview"),
                                     br(),
                                     p("The Tahoe-Central Sierra Initiative (TCSI) is a 2.4-million-acre landscape in the Sierra Nevada that spans numerous watersheds and ecosystems in the Tahoe region. The region, like most of the West, has experienced increased wildfires – both in number and severity – over the last twenty years. Management activities that suppressed natural fire on the landscape has led to overgrown and dense forests that are now at risk of high severity fires. Blue Forest Conservation is a non-profit with the goal to help accelerate the pace and scale of forest restoration and management to reduce wildfire risk. Activities such as mechanical thinning and prescribed burns reduce wildfire risk and ensure that ecosystem services are secured. To raise capital to fund expedited forest restoration activities, Blue Forest tasked students at the Bren School of Environmental Science & Management to help assess stakeholder interest in various ecosystem services, benefits provided for free by the forest, throughout the TCSI region. These services include, carbon storage, water yield, social and cultural connections, among numerous other benefits. The objective of this research was to identify and locate where demand for ecosystem services exists, determine where impacts might have the greatest impact to various stakeholders, and determine how and why stakeholders might prefer to prioritize forest management to ensure that ecosystem services continue to be provide benefits to communities throughout the region. In determining ecosystem services of interest and location, Blue Forest believes that it could better persuade communities and beneficiaries of these ecosystem services to contribute financially to their restoration programs now to reduce the risk of high severity wildfires in the region."),
                                     br(),
                                     br(),
                                     p("To achieve this goal, the students conducted a survey and participatory GIS mapping activities with stakeholder organizations within the TCSI region. Participants indicated the ecosystem service benefits that their organization values most, the impacts to ecosystem services that would be detrimental to their organization’s mission and how to prioritize management to ensure wildfire risk reductions do not jeopardize their interests in the area."),
                                     br(),
                                     br(),
                                     p("This application visualizes survey responses from individuals in the Tahoe-Central Sierra Region. Surveys were conducted as part of a Group Project at the Bren School."),
                                     br(),
                                     br(),
                                     "The map below depicts the study area.",
                                     br(),
                                     plotOutput
                                     ("tahoe_map"),
                                     br(),
                                     br())),
                 
                  # Tab 2: Ecosystem Services Overview 
                  tabPanel("Ecosystem Services Overview",
                           sidebarLayout(
                             sidebarPanel("Learn more about the various ecosystem services",
                                          selectInput(
                                            inputId = "ecosystem_services_checkbox",
                                            label = "Select an option:",
                                            choices = c("Water", "Biodiversity", "Climate Regulation", "Recreation and Cultural Values", "Watershed Services", "General Forest Health"),
                                          ) # end of checkboxGroupInput
                             ),
                             mainPanel(h3("Overview of Ecosystem Services in Region"),
                                       textOutput("intro_reactive"),
                                       uiOutput("intro")
                             ))),
                  
                   # Tab 3: Ecosystem benefits
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
                               Description of how participants were asked to identify regions of interest within our study region",
                               plotOutput("eco_ben_reactive_plot")
                             ) #end of mainPanel
                           ) #end of sidebarLayout
                           ),
                  
                  # Tab 4: Impacts/Risks to ecosystem services
                  tabPanel("Impacts/Risks to Ecosystem Services"),
                  
                  # Tab 5: Management priority areas
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
  
# Tab 2: Ecosystem Service Overview
  output$intro_reactive <- reactive({case_when(
    input$ecosystem_services_checkbox == "Water" ~ "Water quantity and water quality are important ecosystem benefits not only to stakeholders within the TCSI, but users downstream. Photo Credit: Albarubescens, CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Commons.",
    input$ecosystem_services_checkbox == "Biodiversity" ~ "The Sierra Nevada is one of the most biodiverse ecoregions in the United States, with high rates of species richness and endemism. For example, more than 3,000 distinct species of vascular plants are known to inhabit the Sierra Nevada. The Sierra Nevada also hosts a variety of vegetation community types, including alpine meadows, mixed-conifer and single-conifer forests, and chaparral (Murphy et al., 2004). Many of the species found in the Sierra Nevada are endemic, rare, threatened or endangered, primarily due to habitat loss and fragmentation, introduced pests and pathogens, and air pollution concerns (World Wildlife Fund). Protecting the remarkable biodiversity of the Sierra Nevada will require conservation and restoration efforts aimed at controlling these and other significant threats. Photo Credit: Armon, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons",
    input$ecosystem_services_checkbox == "Climate Regulation" ~ "Forest systems are critical for carbon uptake and climate change mitigation. Carbon sequestration, or the capture and storage of carbon dioxide from the atmosphere (Manley et al., 2020), plays a significant role in forest ecology both as a source and sink of carbon dioxide. Through photosynthesis, chlorophyll in the leaves of trees capture CO₂ and reduce its concentration in the atmosphere. However, due to human activities such as deforestation this stored carbon is ultimately released into the atmosphere, reversing the beneficial effects that forest ecosystems provide. It is estimated that the benefits provided from carbon sequestration equates to about $65 per ton, totaling $3.4 billion annually in the U.S (Krieger, 2001). Photo Credit: Albarubescens, CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>, via Wikimedia Commons. Ladybugs can be an organic alternative to pest control.",
    input$ecosystem_services_checkbox == "Recreation and Cultural Values" ~ "Forests provide many recreational and cultural values including but not limited to aesthetic value, tourism, hunting and fishing, and important habitat for endangered species. It is estimated that recreational activities associated with national forest alone contribute to roughly $110 billion annually in the U.S. (Krieger, 2001). There is also value attached to forests in terms of their longevity and knowing that they will provide value in the future. Additionally, the Tahoe-Central Sierra region is the ancestral and current home of the Washoe and Nisenan tribes, and are of great cultural value to these communities. Photo Credit: Armon, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons",
    input$ecosystem_services_checkbox == "Watershed Services" ~ "Forest watersheds trap and store water underground, contributing to the amount of freshwater available across the globe. Forests help purify water by filtering contaminants and other chemicals through their root systems (Manley et al., 2020). Water is required for all forms of life, thus it is important to protect the ecosystems that maintain our water availability and purity. Water flowing from forested watersheds is commonly utilized in many industries such as agriculture, electricity, and municipal water supplies. Water sourced from forest lands are estimated to be valued at around $0.26/acre-foot for electrical use to $50/acre-foot for irrigation and municipal use (Krieger, 2001). Photo Credit: Armon, CC BY-SA 3.0 <http://creativecommons.org/licenses/by-sa/3.0/>, via Wikimedia Commons",
    input$ecosystem_services_checkbox == "General Forest Health" ~ "Overwintering sites are located all along the California coast, and they are where adult Monarch butterflies aggregate in the late fall until the beginning of spring (State of the Monarch Butterfly Overwintering Sites). They are threatened by development and improper management. Photo Credit: Brocken Inaglory, CC BY-SA 3.0 <https://creativecommons.org/licenses/by-sa/3.0>, via Wikimedia Commons")
    
  })
  
  output$intro <- renderUI({
    if(input$ecosystem_services_checkbox == "Water"){img(src = "images/water.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Biodiversity"){img(src= "images/biodiversity.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Climate Regulation"){img(src= "images/climate_regulation.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Recreation and Cultural Values"){img(src= "images/recreation.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Watershed Services"){img(src= "images/watershed_services.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "General Forest Health"){img(src= "images/general_forest_health.png", height="50%", width="50%", align="left")}
  })
  

# Tab 3: Ecosystem benefits  
  eco_ben_reactive <- reactive({
    benefits_tidy_sf %>% 
      filter(ecosystem_benefit %in% input$ecosystem_service)
  }) #end sw_reactive
  
  output$eco_ben_reactive_plot <- renderPlot(
    ggplot(data = eco_ben_reactive()) +
      geom_sf(aes(geometry = geometry, fill = ecosystem_benefit), color = "darkcyan", alpha = 0.5) +
      theme_minimal())
  
# Tab 4:
  
# Tab 5:  
}



###### Combine into an app:

shinyApp(ui = ui, server = server)