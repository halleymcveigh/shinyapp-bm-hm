# attach packages
library(shiny)
library(tidyverse)
library(here)

benefits <- read.csv(here("data", "benefits_categories.csv"))

# create the user interface
ui <- fluidPage(theme = "tahoe.css",
                navbarPage(
                  "Our app title - Blue Forest Conservation in Tahoe",
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
                               "OUTPUT MAP GOES HERE",
                               plotOutput("eco_ben_reactive_plot")
                             ) #end of mainPanel
                           ) #end of sidebarLayout
                           ),
                  tabPanel("Risks to ecosystem services"),
                  tabPanel("Priority areas")
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