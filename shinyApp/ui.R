library(shiny)

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

appCSS <- ".mandatory_star { color: red; }"

# Define UI for data input
ui <- fluidPage(
  # Application title
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel("Field Sampling Input Form"),
  
  textInput('date', labelMandatory("Date"), "yyyy-mm-dd"),
  textInput('samplingTrip', labelMandatory("Sampling Trip")),
  textInput('recorder', labelMandatory("Recorder")),
  textInput('location', labelMandatory("Location")),
  
  # specify sample information
  helpText("SAMPLE DATA: ENTER AS MUCH AS POSSIBLE"),
  textInput('commonName', "Common Name"),
  textInput('localName', "Local  Name"),
  textInput('length_cm', "Length (cm)"),
  textInput('count', "Count at Given Length", value = 1),
  textInput('sex', "Sex"),
  textInput('notes', "Notes"),
  textInput('photoID', "Photo Name/Number"),

  # specify site metadata
  helpText("SAMPLING METADATA: ENTER AS MUCH AS POSSIBLE"),
  textInput('start_time', "Start Time"),
  textInput('stop_time', "Stop Time"),
  textInput('ownership', "EEZ (Economic Exclusion Zone)"),
  textInput('nearestLandmark', "Nearest Landmark"),
  textInput('gear', "Sampling Gear"),
  textInput('bait', "Sampling Bait"),
  textInput('samplersNumber', "Number of Samplers"),
  textInput('depth', "Sampling Depth"),
  
  helpText("Must specify all required fields(*)"),
  actionButton("submit", "Submit"),
  actionButton("resetLast","Clear Last Line"),
  actionButton("reset","Clear All"),
  
  downloadButton('download',"Download the data"),
  
  DT::dataTableOutput("responses", width = 300), tags$hr()
)
