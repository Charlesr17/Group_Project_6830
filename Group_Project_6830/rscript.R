library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

train <- read.csv("train.csv")

ui <- fluidPage(
  
  titlePanel("Housing Data Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #draw ui to select which plot
      radioButtons(
        inputId = "plots",
        label = "Choose plots to display:",
        choices = c(
          "Sale Price",
          "Garage Area vs Price",
          "Overall Quality",
          "Neighborhood",
          "Garage Size"
        )
      ),
      
      #draw checkbox filters in ui
      checkboxGroupInput(
        inputId = "filter",
        label = "Choose filters to apply:",
        choices = c(
          "TwoStory"
        )
      ),
      
      #draw slider in ui for pricerange
      sliderInput(
        inputId = "priceRange",
        label = "Sale Price Range",
        min = min(train$SalePrice),
        max = max(train$SalePrice),
        value = c(min(train$SalePrice), max(train$SalePrice)),
        step = 10000,
        sep = ","
      ),
      
      #draw slider in ui for year range
      sliderInput(
        inputId = "yearRange",
        label = "Year Built",
        min = min(train$YearBuilt),
        max = max(train$YearBuilt),
        value = c(min(train$YearBuilt), max(train$YearBuilt)),
        step = 1
      ),
      
      #draw slider in ui for lot frontage range
      sliderInput(
        inputId = "LotArea",
        label = "Lot Area",
        min = min(train$LotArea),
        max = max(train$LotArea),
        value = c(min(train$LotArea), max(train$LotArea)),
        step = 1000
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    data <- train
    
    # apply price and year range filter
    data <- data[
      data$SalePrice >= input$priceRange[1] &
      data$SalePrice <= input$priceRange[2] &
      data$YearBuilt >= input$yearRange[1] &
      data$YearBuilt <= input$yearRange[2] &
      data$LotArea >= input$LotArea[1] &
      data$LotArea <= input$LotArea[2],
    ]
    
    # check and apply TwoStory filter
    if ("TwoStory" %in% input$filter) {
      data <- data[data$HouseStyle == "2Story", ]
    }
    
    
    #-------- handling which plot to draw:--------
    
    if (input$plots == "Sale Price") {
      ggplot(data, aes(x = SalePrice)) +
        geom_histogram(fill = "steelblue", bins = 40) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Sale Price Distribution",
          x = "Sale Price",
          y = "Count"
        )
      
    } else if (input$plots == "Garage Area vs Price") {
      
      ggplot(data, aes(x = GrLivArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "darkgreen") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Living Area vs Sale Price",
          x = "Living Area",
          y = "Sale Price"
        )
      
    } else if (input$plots == "Overall Quality") {
      
      ggplot(data, aes(x = factor(OverallQual), y = SalePrice)) +
        geom_boxplot(fill = "orange") +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Overall Quality vs Sale Price",
          x = "Overall Quality",
          y = "Sale Price"
        )
      
    } else if (input$plots == "Neighborhood") {
      
      ggplot(data, aes(x = Neighborhood, y = SalePrice)) +
        geom_boxplot(fill = "purple") +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
          title = "Neighborhood vs Sale Price",
          x = "Neighborhood",
          y = "Sale Price"
        )
      
    } else if (input$plots == "Garage Size") {
      
      ggplot(data, aes(x = GarageArea, y = SalePrice)) +
        geom_point(alpha = 0.5, color = "blue") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Garage Area vs Sale Price",
          x = "Garage Area",
          y = "Sale Price"
        )
    }
  })
}

shinyApp(ui, server)