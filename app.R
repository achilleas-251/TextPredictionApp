library(shiny)
library(shinythemes)

source("functions/next_word.R")

# Define UI ----
ui <- fluidPage(
  
      theme = shinytheme("united"),
      
          br(), 
          
          br(),
      
          br(),
      
          "Enter you text. The prediction for the next word will appear below.",
          
          br(),
      
          br(),
          
          textInput("text", NULL, , width = '700px'),
          
          br(),
            
          span(style="color:darkorange",
            strong(h3(textOutput("next_word")))),
      
          img(src='cloud.png'),
      
            align = "center",
         
         br(), 
      
         br(),
      
         br(),
      
         br(),
      
         br(),
      
        "Developed by Achilleas Passias"
      
)

# Define server logic ----
server <- function(input, output) {
  
  output$next_word <- renderText({ 
    next_word_pentagram(input$text)
  })
  
}


shinyApp(ui = ui, server = server)