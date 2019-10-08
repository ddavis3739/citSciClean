##autofill for species name using common names 

##ui.R code

shinyUI(fluidPage(
  titlePanel("Auto Fill"),
  sidebarPanel(
    selectizeInput("p1", choices = Micro_fish$Species, selected = NULL, label = 'Scientific Name'),
    selectizeInput("p2", choices = NULL, label = 'Common Name')
    
  ),
  mainPanel(
    DT::dataTableOutput('table')
  )
)
)


##server.R code

shinyServer(function(input, output, session) {
  
  updateApp <- reactive({
    data <- Micro_fish
    data <- data[data$Species %in% input$p1,]
    updateSelectizeInput(session, 'p2', choices = data$ComNmae, selected = data$ComNmae, server = TRUE)

    data
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(updateApp()) 
  )
  
})
