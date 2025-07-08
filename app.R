library(shiny)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)

data_raw <- read.csv("Annual_14C_Database_V2.csv", stringsAsFactors = FALSE)
Intcal20Curve <- read.csv("Intcal20Curve.csv", stringsAsFactors = FALSE)
SHcal20Curve <- read.csv("SHcal20Curve.csv", stringsAsFactors = FALSE)

Intcal20Curve <- Intcal20Curve %>%
  mutate(
    CAL_BP = suppressWarnings(as.numeric(CAL_BP)),
    Year = ifelse(!is.na(CAL_BP), 1950 - CAL_BP, Year)
  )

SHcal20Curve <- SHcal20Curve %>%
  mutate(
    CAL_BP = suppressWarnings(as.numeric(CAL_BP)),
    Year = ifelse(!is.na(CAL_BP), 1950 - CAL_BP, Year)
  )

data_raw <- data_raw %>%
  mutate(
    Dated_Year = suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", Dated_Year))),
    Age_Corrected_D14C_Error = suppressWarnings(as.numeric(Age_Corrected_D14C_Error)),
    Age_Corrected_D14C = suppressWarnings(as.numeric(Age_Corrected_D14C)),
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude))
  )

ui <- fluidPage(
  titlePanel("14C Data"),
  
  fluidRow(
    column(
      width = 4,
      wellPanel(
        checkboxInput("use_year", "Filter by Year Range", FALSE),
        conditionalPanel(
          condition = "input.use_year == true",
          uiOutput("year_ui")
        ),
        
        checkboxInput("use_country", "Filter by Country", FALSE),
        conditionalPanel(
          condition = "input.use_country == true",
          uiOutput("country_ui")
        ),
        
        checkboxInput("use_latlon", "Filter by Latitude/Longitude", FALSE),
        conditionalPanel(
          condition = "input.use_latlon == true",
          uiOutput("lat_ui"),
          uiOutput("lon_ui")
        ),
        
        checkboxInput("use_authors", "Filter by Authors", FALSE),
        conditionalPanel(
          condition = "input.use_authors == true",
          uiOutput("authors_ui")
        ),
        
        checkboxInput("use_ew", "Filter by EW_LW_WW", FALSE),
        conditionalPanel(
          condition = "input.use_ew == true",
          uiOutput("ew_ui")
        ),
        
        checkboxInput("show_errors", "Show Error Bars", TRUE),
        
        checkboxInput("show_nh_curve", "Show IntCal20 Curve", FALSE),
        
        checkboxInput("show_sh_curve", "Show SHCal20 Curve", FALSE),
        
        br(),
        
        actionButton("reset_filters", "Reset Filters", 
                     class = "btn-sm", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton("download_data", "Download Filtered Data (CSV)", 
                       class = "btn-sm", style = "width: 100%;")
      )
    ),
    column(
      width = 8,
      plotOutput("year_plot", height = 400)
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      div(style = "padding: 0px 20px;",
          leafletOutput("map", height = 600))
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      div(style = "padding: 0px 20px;",
          DTOutput("table"))
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
      numericInput("year_min", "Start Year (Minimum: -5419)", value = min_year, min = min_year, max = max_year),
      numericInput("year_max", "End Year (Maximum: 1933)", value = max_year, min = min_year, max = max_year)
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
      datatable(
        df,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 10
        ),
        class = 'display nowrap'
      )
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
      filter(!is.na(Dated_Year), !is.na(Age_Corrected_D14C))
    
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
    
    min_year <- min(df$Dated_Year, na.rm = TRUE)
    max_year <- max(df$Dated_Year, na.rm = TRUE)
    
    Intcal20Curve_Filtered <- Intcal20Curve %>%
      filter(Year >= min_year, Year <= max_year)
    
    SHcal20Curve_Filtered <- SHcal20Curve %>%
      filter(Year >= min_year, Year <= max_year)
    
    plot <- ggplot(df, aes(x = Jittered_Year, y = Age_Corrected_D14C)) +
      geom_point(alpha = 0.6, color = "darkblue", size = 2) +
      theme_minimal() +
      labs(
        x = "Dated Year (jittered)",
        y = "Δ14C (‰)",
        title = "Δ14C Plot"
      )
    
    if (isTRUE(input$show_errors)) {
      plot <- plot + geom_errorbar(
        aes(ymin = Age_Corrected_D14C - Age_Corrected_D14C_Error,
            ymax = Age_Corrected_D14C + Age_Corrected_D14C_Error),
        width = 0.1, color = "darkblue", alpha = 0.4
      )
    }
    if (isTRUE(input$show_nh_curve) && nrow(Intcal20Curve_Filtered) > 1) {
      plot <- plot + geom_line(data = Intcal20Curve_Filtered, aes(x = Year, y = Delta_14C),
                         color = "red", linewidth = 0.5)
    }
    if (isTRUE(input$show_sh_curve) && nrow(SHcal20Curve_Filtered) > 1) {
      plot <- plot + geom_line(data = SHcal20Curve_Filtered, aes(x = Year, y = Delta_14C),
                         color = "green", linewidth = 0.5)
    }
    plot
    }
  )
  
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
