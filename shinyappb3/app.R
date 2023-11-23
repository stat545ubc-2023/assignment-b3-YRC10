#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(gapminder)
library(DT)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gapminder Data Explorer"),
    
    # Image
    img(src = "worldmap.png", height = "250px", width = "auto"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        # year
        sliderInput("yearRange", "Select Year Range:",
                    min = min(gapminder$year), 
                    max = max(gapminder$year),
                    value = c(min(gapminder$year), max(gapminder$year)),
                    step = 5, sep = ""),
        # continent
        radioButtons("continent", "Select Continent:",
                     choices = c("World", as.character(unique(gapminder$continent)))),
        # property
        selectInput("property", "Select Property:",
                    choices = c("Life Expectancy" = "lifeExp",
                                "Population" = "pop",
                                "GDP per Capita" = "gdpPercap"))
      ),
      mainPanel(
        plotOutput("averagePlot"),
        DTOutput("dataView")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive expression to filter data based on inputs
  filteredData <- reactive({
    if (input$continent == "World") {
      gapminder %>%
        filter(year >= input$yearRange[1], year <= input$yearRange[2])
    } else {
      gapminder %>%
        filter(year >= input$yearRange[1], year <= input$yearRange[2],
               continent == input$continent)
    }
  })
  
  # Render the plot
  output$averagePlot <- renderPlot({
    data <- filteredData() %>%
      group_by(year) %>%
      summarise(Average = mean(as.numeric(get(input$property)), na.rm = TRUE)) # Make sure to convert to numeric and remove NA values
    
    ggplot(data, aes(x = year, y = Average)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Average", input$property, "in", input$continent),
           x = "Year",
           y = paste("Average", input$property))
  })
  
  # Render the data table
  output$dataView <- renderDT({
    data <- filteredData()
    # Dynamically select the required columns
    selectedProperty <- input$property
    data %>% select(year, country, all_of(selectedProperty))
  }, options = list(pageLength = 10)) # Options for the rendered data table
}

# Run the application 
shinyApp(ui = ui, server = server)
