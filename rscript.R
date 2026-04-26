library(shiny)
library(ggplot2)
library(dplyr)

#load data
#also builds checkbox ui
train <- read.csv("train.csv")

ui <- fluidPage(
  
  titlePanel("Ames Housing Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "plots",
        label = "Choose plots to display:",
        choices = c(
          "Sale Price",
          "Living Area vs Price",
          "Overall Quality",
          "Neighborhood",
          "Garage Size"
        )
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

#-----------------------------------------------------------------------------
#drawing plots
#-----------------------------------------------------------------------------
server <- function(input, output) {
  
  
  
  
  output$plot <- renderPlot({
    data <- train
    #full sales price plot
    if ("Sale Price" %in% input$plots) { # this line basically means: did the user select "Sale Price" from check boxes
      
      ggplot(data, aes(x = SalePrice)) +
        geom_histogram(fill = "steelblue", bins = 40) +
        theme_minimal() +
        labs(title = "Sale Price Distribution")
    }
    
    
    
    #living area/price
    else if ("Living Area vs Price" %in% input$plots) {
      
      ggplot(data, aes(x = GrLivArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "darkgreen") +
        theme_minimal() +
        labs(title = "Living Area vs Sale Price")
    }
    
    
    
    #quality/price
    else if ("Overall Quality" %in% input$plots) {
      
      ggplot(data, aes(x = factor(OverallQual), y = SalePrice)) +
        geom_boxplot(fill = "orange") +
        theme_minimal() +
        labs(title = "Overall Quality vs Sale Price",
             x = "Overall Quality",
             y = "Sale Price")
    }
    
    
    
    #neighborhood/price
    else if ("Neighborhood" %in% input$plots) {
      
      ggplot(data, aes(x = Neighborhood, y = SalePrice)) +
        geom_boxplot(fill = "purple") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Neighborhood vs Sale Price",
             x = "Neighborhood",
             y = "Sale Price")
    }
    
    
    
    #garage size/price
    else if ("Garage Size" %in% input$plots) {
      
      ggplot(data, aes(x = GarageArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "blue") +
        theme_minimal() +
        labs(title = "Garage Area vs Sale Price")
    }
  })
}

shinyApp(ui, server)