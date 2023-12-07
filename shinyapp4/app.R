library(shiny)
library(dplyr)
library(ggplot2)
library(gapminder)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gapminder Data Explorer"),
  
  # Dynamic image output
  uiOutput("continentImage"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Year
      sliderInput("yearRange", "Select Year Range:",
                  min = min(gapminder$year), 
                  max = max(gapminder$year),
                  value = c(min(gapminder$year), max(gapminder$year)),
                  step = 5, sep = ""),
      # Continent
      radioButtons("continent", "Select Continent:",
                   choices = c("World", as.character(unique(gapminder$continent)))),
      # Country (dynamic)
      selectInput("country", "Select Country/Region:", choices = NULL),
      # Property
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


# Define server logic
server <- function(input, output, session) {
  
  gapminder <- gapminder %>%
    mutate(country = as.character(country), # Convert to character
           country = ifelse(country == "Taiwan", "Taiwan, China", country)) %>%
    mutate(country = as.factor(country)) # Convert back to factor
  
  output$continentImage <- renderUI({
    # Based on the input, choose the appropriate image file
    continent <- input$continent
    imgFile <- switch(continent,
                      "Asia" = "asia.png",
                      "Europe" = "europe.png",
                      "Africa" = "africa.png",
                      "Americas" = "americas.png",
                      "Oceania" = "oceania.png",
                      "worldmap.png")
    
    # Return the image tag if an image file is selected
    if (!is.null(imgFile)) {
      tags$img(src = imgFile, height = "250px", width = "auto")
    }
  })
  
  
  # Update country selection based on continent selection
  observeEvent(input$continent, {
    if (input$continent == "World") {
      updateSelectInput(session, "country", choices = c("All", as.character(unique(gapminder$country))))
    } else {
      updateSelectInput(session, "country", choices = c("All", as.character(unique(gapminder$country[gapminder$continent == input$continent]))))
    }
  }, ignoreNULL = FALSE)
  
  # Reactive expression to filter data based on inputs
  filteredData <- reactive({
    data <- gapminder %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2])
    if (input$continent != "World") {
      data <- data %>%
        filter(continent == input$continent)
    }
    if (input$country != "All") {
      data <- data %>%
        filter(country == input$country)
    }
    data
  })
  
  # Render the combined plot
  output$averagePlot <- renderPlot({
    # Data for average property
    data_continent <- gapminder %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2],
             if (input$continent != "World") continent == input$continent else TRUE) %>%
      group_by(year) %>%
      summarise(Average = mean(as.numeric(get(input$property)), na.rm = TRUE))
    
    # Base plot with a named aes for the legend
    p <- ggplot() +
      geom_line(data = data_continent, aes(x = year, y = Average, group = 1, color = "Continent Average")) +
      labs(x = "Year", y = input$property)
    
    # Add the country's line if a specific country is selected
    if (input$country != "All") {
      data_country <- gapminder %>%
        filter(year >= input$yearRange[1], year <= input$yearRange[2], country == input$country)
      
      p <- p + geom_line(data = data_country, aes(x = year, y = get(input$property), group = 2, color = "Country Data"))
    }
    
    # Update plot title
    title <- if (input$country == "All" || is.null(input$country)) {
      if (input$continent == "World") "Global Average" else paste("Average in", input$continent)
    } else {
      paste("Data for", input$continent, "and", input$country)
    }
    
    # Add legend and adjust colors
    p + labs(title = title) +
      scale_color_manual(values = c("Continent Average" = "blue", "Country Data" = "red"),
                         name = "Legend",
                         breaks = c("Continent Average", "Country Data"))
  })
  
  
  # Render the data table
  output$dataView <- renderDT({
    data <- filteredData()
    selectedProperty <- input$property
    data %>% select(year, country, all_of(selectedProperty))%>%
    rename("Country/Region" = country) # Rename the country colum
  }, options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
