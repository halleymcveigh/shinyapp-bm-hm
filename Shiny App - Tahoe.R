# attach packages
library(shiny)
library(tidyverse)
library(here)

benefits <- read.csv(here("data", "benefits_categories.csv"))

# create the user interface
ui <- fluidPage(theme = "tahoe.css",
                navbarPage(
                  "Visualizing Ecosystem Services of Interest in the Tahoe-Central Sierra Region - Blue Forest Conservation",
                  tabPanel("Ecosystem benefits",
                           sidebarLayout(
                             sidebarPanel(
                               "WIDGET 1",
                               checkboxGroupInput(inputId = "ecosystem_service",
                                                  label = "Select ecosystem service:",
                                                  choices = unique(benefits$What.forest.benefit.does.your.organization.value.in.this.area.)
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
                  tabPanel("Impacts/Risks to Ecosystem Services"),
                  tabPanel("Threats to Ecosystem Services"),
                  tabPanel("Priority Management Areas")
                ) #end of navbarPage
                ) #end ui


# create server function
server <- function(input, output) {
  
  eco_ben_reactive <- reactive({
    benefits %>% 
      filter(What.forest.benefit.does.your.organization.value.in.this.area. %in% input$ecosystem_service)
  }) #end sw_reactive
  
  output$eco_ben_reactive_plot <- renderPlot(
    ggplot(data = eco_ben_reactive(), aes(x = Recreation, y = Carbon.storage)) +
      geom_point(aes(color = What.forest.benefit.does.your.organization.value.in.this.area.))
  )
}

# combine into an app:

shinyApp(ui = ui, server = server)