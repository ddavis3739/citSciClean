#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

### define functions

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

### list input fields

fieldsMandatory <- c("date", "recorder", "location")

optionalField = c('start_time', 'stop_time', 'ownership', 'gear', 'bait', 'samplersNumber', 'depth', 'amountSampled')





appCSS <- ".mandatory_star { color: red; }"

# Define UI for application that draws a histogram
ui <- fluidPage(
        # Application title
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        titlePanel("Field Sampling Input Form"),
        
        div(
            id = "form",
            
            textInput("date", labelMandatory("Name"), ""),
            textInput("recorder", labelMandatory("Recroder")),
            textInput("location", labelMandatory("Location")),
            actionButton("submit", "Submit", class = "btn-primary")
        )
)

# text inputs 
text_demo <- textInput(
    "fav_band", 
    "What is your favourite 80s band?"
)


# Define server logic required to draw a histogram
server = function(input, output, session) {
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
}


fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")
responsesDir <- file.path("responses")
epochTime <- function() {
    as.integer(Sys.time())
}

formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
})

# Run the application 
shinyApp(ui = ui, server = server)
