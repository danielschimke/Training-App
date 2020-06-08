library(shiny)


ui <- fluidPage(
    
    
    titlePanel("Training"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("text", "Enter text:")
        ),
        
        mainPanel(
            
            textOutput("sample")
        )
    )
)


server <- function(input, output) {
    
    output$sample <- renderText(input$text)
}


shinyApp(ui = ui, server = server)
