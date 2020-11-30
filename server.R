library(shiny)
library(shinyWidgets)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$selected_counties <- renderTable(input$county)
    output$selected_lakes <- renderTable(input$lake)
    output$selected_surveys <- renderTable(input$survey)
    output$selected_fish_species <- renderTable(input$fish_species)
    
    lakes <- reactive({

        req(length(input$county) > 0)
        
        get_lakes(input$county)

    })
    observe(updatePickerInput(session = session,
                              inputId = "lake", 
                              choices = lakes()))
    
    surveys <- reactive({
        
        req(length(input$lake) > 0)
        
        get_survey_headers(input$lake)
        
    })
    observe(updatePickerInput(session = session,
                              inputId = "survey",
                              choices = surveys()))
    
})
