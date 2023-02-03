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

housing$month.sold.name <- factor(housing$month.sold.name, 
                                   levels= rev(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

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
                  choices = c("Sale price" = "sale.prc", 
                              "Land area (sqft)" = "lnd.sqft", 
                              "Floor area (sqft)" = "tot.lvg.area", 
                              "Value of special features" = "spec.feat.val", 
                              "Distance to the ocean" = "ocean.dist", 
                              "Distance to nearest water body" = "water.dist",
                              "Distance to business dist." = "cntr.dist", 
                              "Distance to the highway" = "hw.dist"), 
                  selected = "sale.prc"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Sale price" = "sale.prc", 
                  "Land area (sqft)" = "lnd.sqft", 
                  "Floor area (sqft)" = "tot.lvg.area", 
                  "Value of special features" = "spec.feat.val", 
                  "Distance to the ocean" = "ocean.dist", 
                  "Distance to nearest water body" = "water.dist",
                  "Distance to business dist." = "cntr.dist", 
                  "Distance to the highway" = "hw.dist"), 
                  selected = "lnd.sqft"),
    
     
      # Horizontal line for visual separation -----------------------
      hr(),      
      
      
       # Set Age of the property ------------------------------------
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
        tabPanel("Month Sold Distribution", plotOutput(outputId = "bar.chart",  height = "350px", width = "900px")), #tab for bar chart
        tabPanel("Sale Price Distribution", plotOutput(outputId = "pie.chart",  height = "350px", width = "900px")) #tab for pie chart
    
      ),
      
       # Show data table ---------------------------------------------
        DT::dataTableOutput(outputId = "table")

    )
  )
)

# Define server function required to create the scatter plot ---------
server <- function(input, output) {


  housing.subset <- reactive({
    req(input$property.age) # ensure availability of value before proceeding
    filter(housing, age >= input$property.age[1] & age <= input$property.age[2])
  })
  

  # Create scatter plot ----------------------------------------------
  output$scatterplot <- renderPlot({
    ggplot(data = housing.subset(), aes_string(x = input$x, y = input$y)) +
      geom_point(color = "steelblue") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(x = toTitleCase(str_replace_all(input$x, "\\.", " ")),
           y = toTitleCase(str_replace_all(input$y, "\\.", " "))
      )
    }
  )
  
  # Create Bar Chart -------------------------------------------------
  output$bar.chart <- renderPlot({
    ggplot(data = housing.subset(), aes(x = month.sold.name)) +
      geom_bar(color = 'lightblue', fill = 'lightblue') +
      ggtitle("Number of properties sold per month in 2016") +
      xlab("Month of Sale") +
      ylab("Property Count") +
      theme_classic() +
      coord_flip() +
      geom_text(stat='count', aes(label=..count..), position = position_stack(vjust= 1.03)) + 
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold")) 
  }
  )
  
  # # Create Pie Chart-------------------------------------------------
  output$pie.chart <- renderPlot({
    pie <- housing.subset() %>%
      count(price.range) %>%
      mutate(percent = n/sum(n))
    ggplot(data = pie, aes(x = "", y = percent, fill = price.range)) +
      geom_bar(position = "fill", width = 1, stat = "identity", color = "white") +
      scale_fill_brewer(palette = "Blues", name = "Price Range") +
      geom_text(aes(x = 1.7, label = scales::percent(percent, accuracy = .1)), position = position_stack(vjust = .6)) +
      coord_polar(theta = "y") +
      theme_void()
  }
  )
  
# Print data table------------------------------------------------------
output$table <- DT::renderDataTable(
  if(input$show_data){
    DT::datatable(data = housing.subset()[0:14], 
                  options = list(pageLength = 20), 
                  rownames = FALSE,
                  colnames = c('latitude' = 'lat', 'longitude' = 'long', 'parcel no' = 'parcel.no', 'sale price' = 'sale.prc', 
                  'land area' = 'lnd.sqft', 'floor area' = 'tot.lvg.area', 'special features value' = 'spec.feat.val', 
                  'rail dist' = 'rail.dist', 'ocean dist' = 'ocean.dist', 'water dist' = 'water.dist', 'business center dist' = 'cntr.dist',
                  'sub-center dist' = 'subcntr.di', 'highway dist' = 'hw.dist', 'property age' = 'age'))  %>% 
   formatCurrency('sale price', "$")
    
  }
)

# Download data function------------------------------------------------
output$download.data <- downloadHandler(
  filename = function() {
    paste("housing.miami", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(housing.subset(), file)
  }
)
} 

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)

