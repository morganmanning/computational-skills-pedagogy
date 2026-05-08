# load packages
library(shiny)
library(shinyWidgets)
library(clipr)  # clipboard functionality

# server logic
server <- function(input, output, session) {
  generate_unique_id <- function(id) {
    id_char <- as.character(id)
    hashed_id <- digest::digest(id_char, algo = "sha256")
    unique_id <- substr(hashed_id, 1, 8)
    return(unique_id)
  }
  
  unique_id <- reactive({
    id <- input$id_input
    if (nchar(id) != 8 || !grepl("^[0-9]+$", id)) {
      return("Invalid ID")
    }
    generate_unique_id(id)
  })
  
  output$unique_id_output <- renderText({
    unique_id()
  })
  
  # copy unique ID to clipboard
  observeEvent(input$copy_button, {
    clipr::write_clip(unique_id())
    showNotification("Unique ID copied to clipboard", duration = 3)
  })
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .card {
          background-color: #f8f9fa;
          border-radius: 8px;
          padding: 20px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        
        .input-group {
          margin-bottom: 20px;
        }
        
        .output-group {
          font-weight: bold;
          font-size: 18px;
          display: flex;
          align-items: center;
        }
      ")
    )
  ),
  
  fluidRow(
    column(8, offset = 2,
           div(class = "text-center",
               titlePanel("Unique ID Generator")
           )
    )
  ),
  
  fluidRow(
    column(6, offset = 3,
           div(class = "card",
               div(class = "input-group",
                   numericInput("id_input", "Enter an 8-digit ID:", value = NA, width = "100%")
               ),
               div(class = "output-group",
                   verbatimTextOutput("unique_id_output"),
                   actionButton("copy_button", "Copy", icon = icon("clipboard"))
               )
           )
    )
  )
)

# run it!
shinyApp(ui = ui, server = server)

