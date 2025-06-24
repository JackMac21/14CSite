library(shiny)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(rsconnect)

# Load and clean data
data_raw <- read.csv("Annual_14C_Database - 14C Database.csv", stringsAsFactors = FALSE)

data_raw <- data_raw %>%
  mutate(
    Dated_Year = suppressWarnings(as.numeric(gsub("[^0-9\\-]", "", Dated_Year))),
    Age_Corrected_D14C_Error = suppressWarnings(as.numeric(Age_Corrected_D14C_Error)),
    Age_Corrected_D14C = suppressWarnings(as.numeric(Age_Corrected_D14C)),
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude))
  )

ui <- fluidPage(
  titlePanel("14C Data"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("use_ew", "Filter by EW_LW_WW", FALSE),
      conditionalPanel(
        condition = "input.use_ew == true",
        uiOutput("ew_ui")
      ),
      
      checkboxInput("use_country", "Filter by Country", FALSE),
      conditionalPanel(
        condition = "input.use_country == true",
        uiOutput("country_ui")
      ),
      
      checkboxInput("use_authors", "Filter by Authors", FALSE),
      conditionalPanel(
        condition = "input.use_authors == true",
        uiOutput("authors_ui")
      ),
      
      checkboxInput("use_year", "Filter by Year Range", FALSE),
      conditionalPanel(
        condition = "input.use_year == true",
        uiOutput("year_ui")
      ),
      
      checkboxInput("use_latlon", "Filter by Latitude/Longitude", FALSE),
      conditionalPanel(
        condition = "input.use_latlon == true",
        uiOutput("lat_ui"),
        uiOutput("lon_ui")
      ),
      
      checkboxInput("show_errors", "Show Error Bars", TRUE),
      
      br(),
      actionButton("reset_filters", "Reset Filters"),
      br(), br(),
      downloadButton("download_data", "Download Filtered Data (CSV)")
    ),
    
    mainPanel(
      plotOutput("year_plot", height = 400),
      br(),
      leafletOutput("map", height = 600),
      br(),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateCheckboxInput(session, "use_ew", value = FALSE)
    updateCheckboxInput(session, "use_country", value = FALSE)
    updateCheckboxInput(session, "use_authors", value = FALSE)
    updateCheckboxInput(session, "use_year", value = FALSE)
    updateCheckboxInput(session, "use_latlon", value = FALSE)
  })
  
  output$ew_ui <- renderUI({
    selectizeInput("ew_value", "Select EW_LW_WW",
                   choices = sort(na.omit(unique(data_raw$EW_LW_WW))),
                   multiple = TRUE)
  })
  
  output$country_ui <- renderUI({
    selectizeInput("country_value", "Select Country",
                   choices = sort(na.omit(unique(data_raw$Country))),
                   multiple = TRUE)
  })
  
  output$authors_ui <- renderUI({
    selectizeInput("authors_value", "Select Authors",
                   choices = sort(na.omit(unique(data_raw$Author_Year))),
                   multiple = TRUE)
  })
  
  output$year_ui <- renderUI({
    valid_years <- na.omit(data_raw$Dated_Year)
    min_year <- floor(min(valid_years))
    max_year <- ceiling(max(valid_years))
    
    tagList(
      numericInput("year_min", "Start Year", value = min_year, min = min_year, max = max_year),
      numericInput("year_max", "End Year", value = max_year, min = min_year, max = max_year)
    )
  })
  
  output$lat_ui <- renderUI({
    sliderInput("lat_range", "Latitude Range", 
                min = floor(min(data_raw$Latitude, na.rm = TRUE)), 
                max = ceiling(max(data_raw$Latitude, na.rm = TRUE)), 
                value = c(floor(min(data_raw$Latitude, na.rm = TRUE)), ceiling(max(data_raw$Latitude, na.rm = TRUE))))
  })
  
  output$lon_ui <- renderUI({
    sliderInput("lon_range", "Longitude Range", 
                min = floor(min(data_raw$Longitude, na.rm = TRUE)), 
                max = ceiling(max(data_raw$Longitude, na.rm = TRUE)), 
                value = c(floor(min(data_raw$Longitude, na.rm = TRUE)), ceiling(max(data_raw$Longitude, na.rm = TRUE))))
  })
  
  filtered_data <- reactive({
    df <- data_raw
    
    if (input$use_ew && !is.null(input$ew_value)) {
      df <- df %>% filter(EW_LW_WW %in% input$ew_value)
    }
    
    if (input$use_country && !is.null(input$country_value)) {
      df <- df %>% filter(Country %in% input$country_value)
    }
    
    if (input$use_authors && !is.null(input$authors_value)) {
      df <- df %>% filter(Author_Year %in% input$authors_value)
    }
    
    if (input$use_year) {
      req(input$year_min, input$year_max)
      df <- df %>% filter(!is.na(Dated_Year), Dated_Year >= input$year_min, Dated_Year <= input$year_max)
    }
    
    if (input$use_latlon) {
      req(input$lat_range, input$lon_range)
      df <- df %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude >= input$lat_range[1], Latitude <= input$lat_range[2],
               Longitude >= input$lon_range[1], Longitude <= input$lon_range[2])
    }
    
    df
  })
  
  output$table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      datatable(data.frame(Message = "No records match the filter criteria."), options = list(dom = 't'))
    } else {
      datatable(df, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  
  output$map <- renderLeaflet({
    df <- filtered_data()
    df <- df[!is.na(df$Latitude) & !is.na(df$Longitude), ]
    
    if (nrow(df) == 0) {
      return(leaflet() %>% addTiles() %>% addPopups(0, 0, "No data to display"))
    }
    
    leaflet(df) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, clusterOptions = markerClusterOptions(),
                 popup = ~paste(
                   "<strong>Tree Series:</strong>", Tree_Series, "<br/>",
                   "<strong>Species:</strong>", Species, "<br/>",
                   "<strong>Author/Year:</strong>", Author_Year, "<br/>",
                   "<strong>Dated Year:</strong>", Dated_Year
                 ))
  })
  
  output$year_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Dated_Year), !is.na(Age_Corrected_D14C), !is.na(Age_Corrected_D14C_Error),
             Dated_Year <= 2025)
    
    if (nrow(df) == 0) {
      plot.new()
      title("No data to display")
      return()
    }
    
    if (nrow(df) > 5000) {
      df <- df[sample(nrow(df), 5000), ]
      showNotification("Displaying a random sample of 5000 records for performance.", type = "message")
    }
    
    set.seed(123)
    df$Jittered_Year <- jitter(df$Dated_Year, amount = 0.3)
    
    p <- ggplot(df, aes(x = Jittered_Year, y = Age_Corrected_D14C)) +
      geom_point(alpha = 0.6, color = "darkblue", size = 2) +
      theme_minimal() +
      labs(
        x = "Dated Year (jittered)",
        y = "Age-Corrected Δ14C",
        title = "Δ14C Plot with Optional Error Bars"
      )
    
    if (isTRUE(input$show_errors)) {
      p <- p + geom_errorbar(
        aes(ymin = Age_Corrected_D14C - Age_Corrected_D14C_Error,
            ymax = Age_Corrected_D14C + Age_Corrected_D14C_Error),
        width = 0.1, color = "darkblue", alpha = 0.4
      )
    }
    
    p
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_14C_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)