library(shiny)
library(shinydashboard)
library(datasets)
library(tidyverse)
library(ggplot2)

########################################
###     Graphical User Interface     ###
########################################
ui <- dashboardPage(
    dashboardHeader(title = "Exploring Iris!"),
    dashboardSidebar(
        textInput("text", "Enter text:"),
        textOutput("sample"),
        actionButton("modal", "Open Modal"),
        textOutput("sliderText"),
        actionButton("chartSample", "Create Chart"),
        plotOutput("chart")
    ),
    dashboardBody(
        plotOutput("petal_width"),
        plotOutput("petal_length"),
        plotOutput("sepal_width"),
        plotOutput("sepal_length"),
        tableOutput("summary_table"),
        plotOutput("pie_by_species"),
        plotOutput("sepal_box")
    )
)


#######################################
###         Internal Server         ###
#######################################
server <- function(input, output) {
    output$sample <- renderText(input$text)
    
    observeEvent(input$modal, {
        showModal(modalDialog(
            easyClose = TRUE,
            title = "Number Printer",
            sliderInput("slider", "Pick a Number", min = 1, max = 10, value = 1),
            actionButton("ok", "OK")
        ))
    })
    
    observeEvent(input$ok, {
        output$sliderText <- renderText(input$slider)
        removeModal()
    })
    
    observeEvent(input$chartSample, {
        showModal(modalDialog(
            title = "Chart Test",
            numericInput("xValue", "X Value", value = 0, min = 0),
            selectInput("yValue", "Y Value", c("0" = "0", "1" = "1", "2" = "2")),
            actionButton("submitChart", "OK")
        ))
    })
    
    observeEvent(input$submitChart, {
       xVal <- input$xValue
       yVal <- input$yValue
       yVal2 <- as.numeric(yVal)
       
       newChart <- data.frame(xVal, yVal2)
       #simpleTable <- table(newChart$xVal)
       #simpleTable <- table(mtcars$cyl)
       output$chart <- renderPlot({
           ggplot(newChart) +
               geom_histogram(stat = "identity", aes(x=xVal, y=yVal2)) + 
               coord_cartesian(xlim=c(0,5), ylim = c(0,2))
           
       })
       removeModal()
        
    })
    
    
    output$petal_width <- renderPlot({
        ggplot(iris) +
            geom_histogram(aes(x=Petal.Width, fill=Species))})
    
    output$petal_length <- renderPlot({
        ggplot(iris) +
            geom_histogram(aes(x=Petal.Length, fill=Species))})
    
    output$sepal_width <- renderPlot({
        ggplot(iris) +
            geom_histogram(aes(x=Sepal.Width, fill=Species))})
    
    output$sepal_length <- renderPlot({
        ggplot(iris) +
            geom_histogram(aes(x=Sepal.Length, fill=Species))})
    
    output$summary_table <- renderTable({
        iris %>% 
            group_by(Species) %>%
            summarise(SepalLength_Mean=mean(Sepal.Length), SepalLength_StdDev=sd(Sepal.Length),
                      SepalWidth_Mean=mean(Sepal.Width), SepalWidth_StdDev=sd(Sepal.Width),
                      PetalLength_Mean=mean(Petal.Length), PetalLength_StdDev=sd(Petal.Length),
                      PetalWidth_Mean=mean(Petal.Width), PetalWidth_StdDev=sd(Petal.Width) )
    })
    
    output$pie_by_species <- renderPlot({
        tbl1 <- as.data.frame(table(iris$Species)) %>% 
            rename(Species = Var1)
        ggplot(tbl1, aes(x = "", y = Freq, fill = Species)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            theme_void()
    })
}


shinyApp(ui = ui, server = server)
