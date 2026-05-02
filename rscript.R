library(shiny)
library(ggplot2)
library(dplyr)
library(scales) # for numeric notation

#load data
train <- read.csv("train.csv")

#create ui and checkboxes
ui <- fluidPage(
  
  titlePanel("Housing Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "plots",
        label = "Choose plots to display:",
        choices = c(
          "Sale Price",
          "Garage Area vs Price",
          "Overall Quality",
          "Neighborhood",
          "Garage Size",
          
          
          "Basic filtering Options Below:",
          "PriceBelow400000",
          "TwoStory",
          "BuiltAfter1990"
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
    
    #basic filtering testing
    if (!is.null(input$plots) && "PriceBelow400000" %in% input$plots) {
      data <- data[data$SalePrice <= 400000, ]
    }
    
    if (!is.null(input$plots) && "TwoStory" %in% input$plots) {
      data <- data[data$HouseStyle == "2Story", ]
    }
    
    if (!is.null(input$plots) && "BuiltAfter1990" %in% input$plots) {
      data <- data[data$YearBuilt > 1990, ]
    }
    
    
    #full sales price plot
    if ("Sale Price" %in% input$plots) { #user selected "Sale Price" from check boxes
      
      ggplot(data, aes(x = SalePrice)) +
        geom_histogram(fill = "steelblue", bins = 40) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(title = "Sale Price Distribution")
      
    }
    
    
    
    #living area/price
    else if ("Garage Area vs Price" %in% input$plots) {
      
      ggplot(data, aes(x = GrLivArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "darkgreen") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(title = "Living Area vs Sale Price")
      
    }
    
    
    
    #quality/price
    else if ("Overall Quality" %in% input$plots) {
      ggplot(data, aes(x = factor(OverallQual), y = SalePrice)) +
        geom_boxplot(fill = "orange") +
        theme_minimal() +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(title = "Overall Quality vs Sale Price",
             x = "Overall Quality",
             y = "Sale Price")
      
    }
    
    
    
    #neighborhood/price
    else if ("Neighborhood" %in% input$plots) {
      ggplot(data, aes(x = Neighborhood, y = SalePrice)) +
        geom_boxplot(fill = "purple") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(title = "Neighborhood vs Sale Price",
             x = "Neighborhood",
             y = "Sale Price")
      
    }
    
    
    
    #garage size/price
    else if ("Garage Size" %in% input$plots) {
      ggplot(data, aes(x = GarageArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "blue") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(title = "Garage Area vs Sale Price")
      
    }
    
    # default fallback graph
    else {
      ggplot(data, aes(x = SalePrice)) +
        geom_histogram(fill = "blue", bins = 40) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(title = "Filtered result")
    }
    
  })
}

shinyApp(ui, server)