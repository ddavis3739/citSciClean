library(shiny)

# which fields get saved 
fieldsAll <- c("date", "samplingTrip", "recorder", "location", 
               'commonName', 'sciName', 'length_cm', 'count', 'sex', 'notes', 'photoID', 
               'start_time', 'stop_time', 'ownership', 'nearestLandmark', 'gear', 
               'bait', 'samplersNumber', 'depth' 
               )

# which fields are mandatory
fieldsMandatory <- c("date", "samplingTrip", "recorder", "location")


# functions
outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

server = function(input, output, session) {
  
  updateApp <- reactive({
    # read in fish data
    Micro_fish <- read.csv("data/Micro_fish.csv", stringsAsFactors = FALSE)
    
    data <- Micro_fish
    data <- data[data$ComName %in% input$p1,]
    updateSelectizeInput(session, 'p2', choices = data$Species, selected = data$ComName, server = TRUE)
    
    data
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(updateApp()) 
  )
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # clear last responce
  observeEvent(input$resetLast,{
    #Define the file name that will be deleted
    df <- file.info(list.files("./responses/", full.names = T))
    fn = rownames(df)[which.max(df$mtime)]
    #Check its existence
    if (file.exists(fn)) 
      #Delete file if it exists
      file.remove(fn)
  })
  
  observeEvent(input$reset,{
    file.remove(dir(  
      'responses/', 
      pattern = "*", 
      full.names = TRUE
    ))
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    input$resetLast
    input$reset
    loadData()
  })
  
  # download table
  # Reactive expression with the data, in this case iris
  react_data <- reactive(loadData())
  
  output$dto <- renderDataTable({react_data()})
  output$download <- downloadHandler(
    filename = function() {
      paste("Sampling-data-", Sys.Date(), ".csv", sep="")
    }, 
    content = function(filename){
      write.csv(react_data(), filename, row.names=FALSE)
    }
  )
  
}