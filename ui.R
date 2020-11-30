library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        # Application title
        titlePanel("MN DNR Fishery Survey"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                pickerInput(inputId = "county",
                            label = "County",
                            choices = counties, 
                            multiple = T),
                pickerInput(inputId = "lake",
                            label = "Lake",
                            choices = NA,
                            multiple = T),
                pickerInput(inputId = "survey",
                            label = "Survey",
                            choices = NA,
                            multiple = T),
                pickerInput(inputId = "fish_species",
                            label = "Fish Species",
                            choices = fish_species,
                            multiple = T)
            ),
            mainPanel(
                tableOutput("selected_counties"),
                tableOutput("selected_lakes"),
                tableOutput("selected_surveys"),
                tableOutput("selected_fish_species")
            )
        )
    )
)

