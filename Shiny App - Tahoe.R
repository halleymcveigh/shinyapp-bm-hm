# attach packages
library(shiny)
library(tidyverse)
library(here)
library(tmap)
library(sf)
library(janitor)
library(sp)
library(jpeg)
library(shinythemes)
library(maps)
library(mapdata)
library(ggmap)


# Read in data for spatial analysis
benefits_sf <- read_sf(here("data", "benefits_polygons.shp")) %>% 
  clean_names() %>% 
  select(c(14:33,40:41))

impacts_sf <- read_sf(here("data", "adverse_impacts_polygons.shp")) %>% 
  clean_names() %>% 
  select(c())

mgmt_sf <- read_sf(here("data", "mgmt_prioritization_polygons.dbf")) %>% 
  select(c(13:26,33))


# Read in csv files
benefits <- read_csv(here("data", "benefits_polygons.csv"))

impacts <- read_csv(here("data", "adverse_impacts_polygons.csv"))

mgmt <- read_csv(here("data", "mgmt_prioritization_polygons.csv"))


# Wrangle benefits data
tmap_mode("view")

# Read in data for spatial analysis
benefits_sf <- read_sf(here("data", "benefits_polygons.shp")) %>% 
  clean_names() %>% 
  select(c(14:33,40:41)) %>% 
  rename(
    "Biodiversity" = "biodiversi",
    "Carbon storage" = "carbon_sto",
    "Commercial real estate" = "commercial",
    "Cultural connections" = "cultural_c",
    "Electric power generation" = "electric_p",
    "Foraging" = "foraging_a",
    "Forest products" = "forest_pro",
    "General forest health" = "general_fo",
    "Invesetment property" = "investment",
    "Landscape aesthetic" = "landscape",
    "Local climate regulation" = "local_clim",
    "Local community connections" = "local_comm",
    "Meadow health" = "meadow_hea",
    "Public health" = "public_hea",
    "Recreation" = "recreation",
    "Residential property" = "residentia",
    "Spritual" = "spiritual",
    "Water supply" = "water_supp",
    "Water quality" = "water_qual"
  )

benefits_tidy_sf <- benefits_sf %>% 
  pivot_longer(c(1:19), names_to = "ecosystem_benefit", values_to = "count") %>% 
  drop_na() %>% 
  select(respondent = responde_1, association = responde_8, ecosystem_benefit, count, geometry)


# Make benefits maps
tahoe_gg <- ggplot(data = benefits_tidy_sf) +
  geom_sf(aes(geometry = geometry, fill = ecosystem_benefit), color = "darkcyan") +
  theme_minimal()

tahoe_gg


#plotly(tahoe_gg)

benefits_tidy_sf = st_as_sf(benefits_tidy_sf)

class(benefits_tidy_sf)

tm_basemap("Stamen.Watercolor")
tmap_options(check.and.fix = TRUE)

#tm_shape(benefits_tidy_sf) + tm_polygons()


# Tahoe map watercolor
tahoe_map <- get_stamenmap(bbox = c(left = -121.5,
                                    bottom = 38,
                                    right = -118.5,
                                    top = 40.5),
                           maptype = "watercolor",
                           crop = FALSE)
ggmap(tahoe_map) +
  theme_void()

# Tahoe basemap terrain
tahoe_basemap <- get_stamenmap(bbox = c(left = -121.5,
                                        bottom = 38,
                                        right = -118.5,
                                        top = 40.5),
                               maptype = "terrain-background",
                               crop = FALSE)

# Benefits map with watercolor basemap
ggmap(tahoe_map) +
  geom_sf(data = benefits_tidy_sf, aes(geometry = geometry, fill = ecosystem_benefit), alpha = 0.5,
          inherit.aes = FALSE) +
  theme_minimal()



# Impacts data wrangling
impacts_subset <- impacts %>% 
  select(c(16:36)) %>% 
  clean_names() %>% 
  rename(
    "Water impacts" = "water_impacts",
    "Sedimentation" = "sedimentation",
    "Public safety and human helth" = "public_safey_and_human_health",
    "Loss of stored carbon" = "loss_of_stored_carbon",
    "Loss of residential property" = "loss_of_residential_property",
    "Loss of recreational income" = "loss_of_recreational_income",
    "Loss of recreation opportunities" = "loss_of_recreation_opportunities",
    "Loss of local climate regulation" = "loss_of_local_climate_regulation",
    "Loss of landscape beauty" = "loss_of_landscape_beauty",
    "Loss of forest products" = "loss_of_forest_products",
    "Loss of forage or food" = "loss_of_forage_or_food",
    "Loss of culturally or spiritually important places" = "loss_of_culturally_or_spiritually_important_places",
    "Aquatic habitat" = "aquatic_habitat",
    "Sedimentation" = "sedimentation",
    "Loss of community or social connections from displacement" = "loss_of_community_or_social_connections_from_displacement",
    "General forest loss" = "general_forest_loss",
    "Loss of biodiversity" = "loss_of_biodiversity",
    "Loss of commercial property or infrastructure" = "loss_of_commercial_property_or_infrastructure",
    "Debris flow" = "debris_flow",
    "Loss of investment property" = "loss_of_investment_property",
    "High cost of emergency services" = "high_cost_of_emergency_services"
  ) %>% 
  pivot_longer(c(1:20), names_to = "impacts", values_to = "count") %>% 
  drop_na()

summary_impacts <- impacts_subset %>% 
  group_by(impacts, org_type) %>% 
  summarize(n = n())

impacts_plot <- ggplot(data = summary_impacts) +
  geom_bar(aes(x = impacts, fill = org_type)) +
  coord_flip() +
  theme_minimal()

impacts_plot


## Wrangling management sf data

mgmt_tidy_sf <- mgmt_sf %>% 
  rename(
    "Wildfire management" = "wildfire.m",
    "Community protection" = "community",
    "Biodiversity" = "biodiversi",
    "Forest health" = "forest.hea",
    "Restoration" = "restoratio",
    "Prescribed burns" = "prescribed",
    "Water quantity" = "water.quan",
    "Headwaters" = "headwaters",
    "Current wildfire risk" = "current.wi",
    "High fuel load" = "current.hi",
    "Infrastructure protection" = "infrastruc",
    "Residential property" = "residentia",
    "Aquatic habitat" = "aquatic.ha"
  ) %>% 
  pivot_longer(c(1:13), names_to = "management_concern", values_to = "count") %>% 
  filter(count >= 1) 
  #drop_na()

## Management map with terrain basemap
mgmt_map <- ggmap(tahoe_basemap) +
  geom_sf(data = mgmt_tidy_sf, aes(geometry = geometry), alpha = 0.5,
          inherit.aes = FALSE) +
  theme_minimal()

mgmt_map
#ggplotly(mgmt_map)





###### Create the user interface
ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Ecosystem Services of Interest in the Tahoe-Central Sierra Region - Blue Forest Conservation"),
                navbarPage(
                  "Explore the Tahoe Basin",
                  
                  # Tab 1: Introduction
                  tabPanel("Overview",
                           mainPanel(h3("Study Overview"),
                                     br(),
                                     p("The Tahoe-Central Sierra Initiative (TCSI) is a 2.4-million-acre landscape in the Sierra Nevada that spans numerous watersheds and ecosystems in the Tahoe region. The region, like most of the West, has experienced increased wildfires – both in number and severity – over the last twenty years. Management activities that suppressed natural fire on the landscape has led to overgrown and dense forests that are now at risk of high severity fires. Blue Forest Conservation is a non-profit with the goal to help accelerate the pace and scale of forest restoration and management to reduce wildfire risk. Activities such as mechanical thinning and prescribed burns reduce wildfire risk and ensure that ecosystem services are secured. To raise capital to fund expedited forest restoration activities, Blue Forest tasked students at the Bren School of Environmental Science & Management to help assess stakeholder interest in various ecosystem services, benefits provided for free by the forest, throughout the TCSI region. These services include, carbon storage, water yield, social and cultural connections, among numerous other benefits. The objective of this research was to identify and locate where demand for ecosystem services exists, determine where impacts might have the greatest impact to various stakeholders, and determine how and why stakeholders might prefer to prioritize forest management to ensure that ecosystem services continue to be provide benefits to communities throughout the region. In determining ecosystem services of interest and location, Blue Forest believes that it could better persuade communities and beneficiaries of these ecosystem services to contribute financially to their restoration programs now to reduce the risk of high severity wildfires in the region."),
                                     br(),
                                     img(src = "TCSImap.jpeg", height = 475, width = 475),
                                     br(),
                                     p("To achieve this goal, the students conducted a survey and participatory GIS mapping activities with stakeholder organizations within the TCSI region. Participants indicated the ecosystem service benefits that their organization values most, the impacts to ecosystem services that would be detrimental to their organization’s mission and how to prioritize management to ensure wildfire risk reductions do not jeopardize their interests in the area."),
                                     br(),
                                     p("This application visualizes survey responses from individuals in the Tahoe-Central Sierra Region. Surveys were conducted as part of a Group Project at the Bren School."),
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
                               "Benefits Received",
                               checkboxGroupInput(inputId = "ecosystem_service",
                                                  label = "Select ecosystem service:",
                                                  choices = unique(benefits_tidy_sf$ecosystem_benefit)
                                                  ) # end checkboxGroupInput
                             ), #end of sidebarPanel
                             mainPanel(h3("Ecosystem Services Benefits"),
                                       br(),
                                       p("The Tahoe-Central Sierra region is a diverse landscape that covers approximately 2.4 million acres of land, multiple National Forests, Wildernesses, recreational opportunities and cities. The region is rich in diversity, but under threat from climate change impacts. This study sought to determine specific stakeholder interest in various ecosystem services to better prioritize how to protect these services under changing climate and future management scenarios. Participants were invited to participate in an interview which incorporated the use of a participatory GIS mapping tool, called Maptionnaire. Participants would be asked a series of questions about the benefits and risks to benefits they perceive within the study region. They were then asked to identify where and which benefits matter to them by drawing a polygon and describing each benefit received in this polygon."),
                                       br(),
                               plotOutput("eco_ben_reactive_plot")
                             ) #end of mainPanel
                           ) #end of sidebarLayout
                           ),
                  
                  
                  # Tab 4: Impacts/Risks to ecosystem services
                  tabPanel("Impacts and Risks to Ecosystem Services",
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput(inputId = "organization_type",
                                                  label = "Select organization type:",
                                                  choices = unique(summary_impacts$org_type)
                                                  )
                             ),
                             mainPanel(h3("Impacts of Concern by Organization Type"),
                                       br(),
                                       p("Participants were asked to identify the greatest impacts that would impact their organizations mission. The impacts listed below might prove helpful to better prioritize forest restoration where specific impact outcomes could be mitigated."),
                                       br(),
                                       plotOutput(
                                         "impacts_plot_reactive"
                                       ),
                                       "This figure explores the types of potential impacts to ecosystem benefits that survey respondents included.")
                           )),
                  
                  
                  
                  # Tab 5: Management priority areas
                  tabPanel("Priority Management Areas",
                           sidebarLayout(
                             sidebarPanel("View priority managment areas by management interest",
                                          selectInput(
                                            inputId = "management_checkbox",
                                            label = "Select an option:",
                                            choices = c("Wildfire management",
                                                        "Community protection",
                                                        "Biodiversity",
                                                        "Forest health",
                                                        "Restoration",
                                                        "Prescribed burns",
                                                        "Water quantity",
                                                        "Headwaters",
                                                        "Current wildfire risk",
                                                        "High fuel load",
                                                        "Infrastructure protection",
                                                        "Residential property",
                                                        "Aquatic habitat"),
                                          ) # end of checkboxGroupInput
                             ),
                             mainPanel(h3("Visualization of Management Priority Areas in Region"),
                                       br(),
                                       p("Participants were asked to draw polygons where their organization would prioritize forest management and restoration activities to secure ecosystem service benefits. Unsurpisingly, most participants highlighted regions that are immediate concern to their organization or its stakeholders. These interviews did provide interesting insights into where local stakeholders believe forest management is severely lacking. Individuals highlighted there reasons for highlighting specific polygons on the map."),
                                       br(),
                                       plotOutput("mgmt_reactive_plot")
                             )))
                  
                  
                ) #end of navbarPage
                ) #end ui


###### Create server function
server <- function(input, output) {

  
# Tab 1: Introduction
  ({
   
  })
  
# Tab 2: Ecosystem Service Overview
  output$intro_reactive <- reactive({case_when(
    input$ecosystem_services_checkbox == "Water" ~ "Water quantity and water quality are important ecosystem benefits not only to stakeholders within the TCSI, but users downstreams and the environment. Water plays a critical role in providing needed habitat for aquatic species. Photo Credit: Larry Miller, CC BY-SA 2.0.",
    input$ecosystem_services_checkbox == "Biodiversity" ~ "The Sierra Nevada is one of the most biodiverse ecoregions in the United States, with high rates of species richness and endemism. For example, more than 3,000 distinct species of vascular plants are known to inhabit the Sierra Nevada. The Sierra Nevada also hosts a variety of vegetation community types, including alpine meadows, mixed-conifer and single-conifer forests, and chaparral (Murphy et al., 2004). Many of the species found in the Sierra Nevada are endemic, rare, threatened or endangered, primarily due to habitat loss and fragmentation, introduced pests and pathogens, and air pollution concerns (World Wildlife Fund). Protecting the remarkable biodiversity of the Sierra Nevada will require conservation and restoration efforts aimed at controlling these and other significant threats Photo Credit: Sylvia Hunt/Audubon Photography Awards",
    input$ecosystem_services_checkbox == "Climate Regulation" ~ "Forest systems are critical for carbon uptake and climate change mitigation. Carbon sequestration, or the capture and storage of carbon dioxide from the atmosphere (Manley et al., 2020), plays a significant role in forest ecology both as a source and sink of carbon dioxide. Through photosynthesis, chlorophyll in the leaves of trees capture CO₂ and reduce its concentration in the atmosphere. However, due to human activities such as deforestation this stored carbon is ultimately released into the atmosphere, reversing the beneficial effects that forest ecosystems provide. It is estimated that the benefits provided from carbon sequestration equates to about $65 per ton, totaling $3.4 billion annually in the U.S (Krieger, 2001).",
    input$ecosystem_services_checkbox == "Recreation and Cultural Values" ~ "Forests provide many recreational and cultural values including but not limited to aesthetic value, tourism, hunting and fishing, and important habitat for endangered species. It is estimated that recreational activities associated with national forest alone contribute to roughly $110 billion annually in the U.S. (Krieger, 2001). There is also value attached to forests in terms of their longevity and knowing that they will provide value in the future. Additionally, the Tahoe-Central Sierra region is the ancestral and current home of the Washoe and Nisenan tribes, and are of great cultural value to these communities.",
    input$ecosystem_services_checkbox == "Watershed Services" ~ "Forest watersheds trap and store water underground, contributing to the amount of freshwater available across the globe. Forests help purify water by filtering contaminants and other chemicals through their root systems (Manley et al., 2020). Water is required for all forms of life, thus it is important to protect the ecosystems that maintain our water availability and purity. Water flowing from forested watersheds is commonly utilized in many industries such as agriculture, electricity, and municipal water supplies. Water sourced from forest lands are estimated to be valued at around $0.26/acre-foot for electrical use to $50/acre-foot for irrigation and municipal use (Krieger, 2001).",
    input$ecosystem_services_checkbox == "General Forest Health" ~ "Forest Health is an important service that is often overlooked. The overall health of a forest ensures the provisioning of numerous other ecosystem services, such as water quantity and quality, carbon sequestration, and biodiversity")
    
  })
  
  output$intro <- renderUI({
    if(input$ecosystem_services_checkbox == "Water"){img(src = "water.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Biodiversity"){img(src= "biodiversity.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Climate Regulation"){img(src= "climate_regulation.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Recreation and Cultural Values"){img(src= "recreation.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "Watershed Services"){img(src= "watershed_services.jpeg", height="50%", width="50%", align="left")}
    else if (input$ecosystem_services_checkbox == "General Forest Health"){img(src= "general_forest_health.png", height="50%", width="50%", align="left")}
  })
  

# Tab 3: Ecosystem benefits  
  eco_ben_reactive <- reactive({
    benefits_tidy_sf %>% 
      filter(ecosystem_benefit %in% input$ecosystem_service)
  }) #end sw_reactive
  
  output$eco_ben_reactive_plot <- renderPlot(
    ggmap(tahoe_map) +
      geom_sf(data = eco_ben_reactive(), aes(geometry = geometry, fill = ecosystem_benefit), color = "darkcyan", alpha = 0.5, inherit.aes = FALSE) +
      theme_minimal())
  
  
# Tab 4: Impacts to ecosystem benefits
  impacts_reactive <- reactive({
    summary_impacts %>% 
      filter(org_type %in% input$organization_type)
  })
  
  output$impacts_plot_reactive <- renderPlot(
    ggplot(data = impacts_reactive()) +
      geom_bar(aes(x = impacts, fill = org_type)) +
      theme_minimal() +
      labs(y = "Count of responses",
           fill = "Organization type") +
      coord_flip()
  )
  
# Tab 5:  Management priority areas
  mgmt_reactive <- reactive({
    message("if management_checkbox is selected", input$management_checkbox)
    x <- mgmt_tidy_sf %>% 
      filter(management_concern %in% input$management_checkbox)
    print(class(x))
    return(x)
  }) #end sw_reactive
  
  output$mgmt_reactive_plot <- renderPlot(
    ggmap(tahoe_basemap) +
      geom_sf(data = mgmt_reactive(), aes(geometry = geometry, fill = management_concern), alpha = 0.5, inherit.aes = FALSE) +
      theme_minimal())
  
  }



###### Combine into an app:

shinyApp(ui = ui, server = server)