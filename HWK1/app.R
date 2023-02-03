library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(dplyr)
library(scales)

#Importing Data

housing <- read.csv("miami_housing_data.csv") 
print(colnames(housing))

#Changing Column names 
colnames(housing) <- c("lat","long","parcel.no","sale.prc","lnd.sqft", "tot.lvg.area", "spec.feat.val", 
                    "rail.dist", "ocean.dist", "water.dist", "cntr.dist", "subcntr.di", "hw.dist", "age", 
                    "avno60plus", "month.sold", "struct.quality")

housing$price.range <- 0 

housing$price.range[housing$sale.prc < 100000] <- "Less than $100,000"
housing$price.range[housing$sale.prc >= 100000 & housing$sale.prc <= 250000] <- "$100,000 - $250,000"
housing$price.range[housing$sale.prc > 250000 & housing$sale.prc <= 5000000] <- "$250,001 - $500,000"
housing$price.range[housing$sale.prc > 500000 & housing$sale.prc <= 1000000] <- "$500,001 - $1,000,000"
housing$price.range[housing$sale.prc > 1000000] <- "More than $1,000,000"


housing$month.sold.name <- 0

housing$month.sold.name[housing$month.sold == 1] <- "January"
housing$month.sold.name[housing$month.sold == 2] <- "February"
housing$month.sold.name[housing$month.sold == 3] <- "March"
housing$month.sold.name[housing$month.sold == 4] <- "April"
housing$month.sold.name[housing$month.sold == 5] <- "May"
housing$month.sold.name[housing$month.sold == 6] <- "June"
housing$month.sold.name[housing$month.sold == 7] <- "July"
housing$month.sold.name[housing$month.sold == 8] <- "August"
housing$month.sold.name[housing$month.sold == 9] <- "September"
housing$month.sold.name[housing$month.sold == 10] <- "October"
housing$month.sold.name[housing$month.sold == 11] <- "November"
housing$month.sold.name[housing$month.sold == 12] <- "December"

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("Miami Housing Market 2016"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Sale Price" = "sale.prc", 
                              "Land area (sqft)" = "lnd.sqft", 
                              "Floor Area (sq ft)" = "tot.lvg.area", 
                              "Value of Special Features" = "spec.feat.val", 
                              "Distance to the Ocean" = "ocean.dist", 
                              "Distance to nearest water body" = "water.dist",
                              "Distance to Business Dist." = "cntr.dist", 
                              "Distance to the Highway" = "hw.dist"), 
                  selected = "sale.prc"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Sale Price" = "sale.prc", 
                  "Land area (sqft)" = "lnd.sqft", 
                  "Floor Area (sq ft)" = "tot.lvg.area", 
                  "Value of Special Features" = "spec.feat.val", 
                  "Distance to the Ocean" = "ocean.dist", 
                  "Distance to nearest water body" = "water.dist",
                  "Distance to Business Dist." = "cntr.dist", 
                  "Distance to the Highway" = "hw.dist"), 
                  selected = "lnd.sqft"),
    
      # Set Age of the property ---------------------------------------------
      sliderInput(inputId = "property.age",
                  label = "Select age of the property:", 
                  min = 0, max = 96, 
                  value = c(0, 10)),
      
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE), 
      
      
      # Add Download Button
      downloadButton("download.data", "Download"),
      h6("Press the download button to save the dataset.")
      
    ),
    
    
    
    
    # Output --------------------------------------------------------
    mainPanel(
      
      # Tabs to separate each graph
      tabsetPanel(
        type="tab",
        tabPanel("Market Analysis", plotOutput(outputId = "scatterplot", height = "350px", width = "900px")), #tab for scatter plot
        tabPanel("Month Sold Distributrion", plotOutput(outputId = "bar.chart",  height = "350px", width = "900px")), #tab for bar chart
        tabPanel("Price Range Distributrion", plotOutput(outputId = "pie.chart",  height = "350px", width = "900px")) #tab for pie chart
    
      ),
      
        # Show data table ---------------------------------------------
        DT::dataTableOutput(outputId = "table")

    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {

  # 
  # housing.subset <- reactive({
  #   req(input$property.age) # ensure availablity of value before proceeding
  #   filter(housing, age %in% input$property.age)
  # })
  # 

  # Create scatterplot --
  output$scatterplot <- renderPlot({
    ggplot(data = housing, aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")))
    
    }
  )

  
  # # Create barchart
  output$bar.chart <- renderPlot({
    ggplot(data = housing, aes(x = month.sold.name)) +
      geom_bar(color = 4, fill = "4") +
      ggtitle("Number of Properties Sold per month in year 2016") +
      xlab("Month of Sale") +
      ylab("Property Count") +
      theme_classic() +
      coord_flip() + 
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold")) 
  }
  )
  
  
  # # Create pie chart object the plotOutput function is expecting --
  output$pie.chart <- renderPlot({
    pie <- housing %>%
      count(price.range) %>%
      mutate(percent = n/sum(n))
    ggplot(data = pie, aes(x = "", y = percent, fill = price.range)) +
      geom_bar(position = "fill", width = 1, stat = "identity", color = "white") +
      geom_text(aes(x = 1.6, label = scales::percent(percent, accuracy = .1)), position = position_stack(vjust = .5)) +
      coord_polar(theta = "y")+
      theme_void()

  }
  )
  
# Print data table if checked -------------------------------------
output$table <- DT::renderDataTable(
  if(input$show_data){
    DT::datatable(data = housing[0:14], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  }
)

output$download.data <- downloadHandler(
  filename = function() {
    paste("housing.miami", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(housing, file)
  }
)
} 

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

