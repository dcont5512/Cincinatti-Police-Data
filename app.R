# Author: Dominic Contreras
# Assignment: Project 2
# Course: R Shiny for Operations Management
# Date: October 15, 2018

# load required libraries
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(lubridate)
library(shinyWidgets)
library(RSocrata)
library(jsonlite)
library(ggplot2)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(stringr)
library(mapview)
library(formattable)
library(scales)
library(shinyalert)
library(data.table)

# read in app token
token <- jsonlite::fromJSON("token3.json")$token

# pull unique values from 'use of force' data to use as input selectors
dat <- read.socrata("https://data.cincinnati-oh.gov/resource/e2va-wsic.json",
                    app_token = token)
neighbName <- sort(unique(dat$sna_neighborhood))
incidentDesc <- sort(unique(dat$incident_description)) 
officerGend <- sort(unique(dat$officer_gender))
officerRace <- sort(unique(dat$officer_race))
suspectGend <- sort(unique(dat$subject_gender))
suspectRace <- sort(unique(dat$subject_race))
dateDefault <- range(dat$incident_date, na.rm = T)
dateMin <- substr(dateDefault[1], start = 1, stop = 10)
dateMax <- substr(dateDefault[2], start = 1, stop = 10)
remove(dateDefault)
remove(dat)

# read in cincinnati neighborhood boundary shapefile
cinciNeighb <- readOGR("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")

# create market color palette
pal <- c("red", "darkred", "lightred", "orange", 
         "beige", "green", "darkgreen", "lightgreen", 
         "blue", "darkblue", "lightblue", "purple", 
         "darkpurple", "pink", "cadetblue", "white", 
         "gray", "lightgray", "black")

# format title and data source notification
header <- dashboardHeader(title = "Cincinnati Police Data",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Source: Open Data Cincinnati", 
                                                        icon = icon("fa fa-exclamation-triangle"))
                          )
)

# side bar layout 
sidebar <- dashboardSidebar(
  sidebarMenu( # page switch between data visualizations and data download
    id = "tabs",
    menuItem("See Data", icon = icon("eye"), tabName = "map"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    
    # neighborhood selector
    selectizeInput("neighbSelect", 
                   "Neighborhoods:", 
                   choices = neighbName, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select neighborhood(s)')),
    
    # incident description selector
    selectizeInput("incSelect", 
                   "Incident Type:", 
                   choices = incidentDesc, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select incident type(s)')),
    
    # officer gender selector
    selectizeInput("offGendSelect", 
                   "Officer Gender:", 
                   choices = c(officerGend), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select officer gender(s)')),
    
    # officer race selector
    selectizeInput("offRaceSelect", 
                   "Officer Race:", 
                   choices = c(officerRace), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select officer race(s)')),
    
    # suspect gender selector
    selectizeInput("susGendSelect", 
                   "Suspect Gender:", 
                   choices = c(suspectGend), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select subject gender(s)')),
    
    # suspect race selector
    selectizeInput("susRaceSelect", 
                   "Suspect Race:", 
                   choices = c(suspectRace), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select subject race(s)')),
    
    # date range selector
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = Sys.Date()-365, end = dateMax, 
                   min = dateMin, max = dateMax, 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    
    # reset selector
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)

# tab layout for plots
body <- dashboardBody(
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #C6011F;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #C6011F;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #C6011F;
                            }        
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #404040;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #C6011F;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #000000;
                            color: #ffffff;
                            }
                            /* toggle button when hovered  */                    
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #C6011F;
                            }
                            '))),
  
  # hide red error messages - only use on final deployment
  # tags$style(type="text/css",
  #   ".shiny-output-error { visibility: hidden; }",
  #   ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  tabItems(
    
    # create viz pages 
    tabItem("map",
            fluidRow(
              useShinyalert(),
              tabBox(width = 12, height = 200,
                     
                     # layout for viz 1 - map
                     tabPanel("Where are police using force?", 
                              HTML("<p><em>The map below shows locations where police officers used force based on the parameters selected.&nbsp;</em></p>"),
                              leafletOutput("plot_map"),
                              div(style = "height:5px"),
                              actionButton("reset_button", "Reset view")),
                     
                     # layout for viz 2 - barchart
                     tabPanel("Who are police using force against?",
                              HTML("<p><em>The graph below shows demographic information about officers and subjects, based on the parameters selected.&nbsp;</em></p>"),
                              radioButtons("fillSelect", 
                                           "What would you like bars to be segmented by?", 
                                           choices = c("Officer Race" = "officer_race", 
                                                       "Officer Gender" = "officer_gender",
                                                       "Subject Race" = "subject_race",
                                                       "Subject Gender" = "subject_gender"), 
                                           selected = "subject_race", 
                                           inline = TRUE,
                                           width = NULL),
                              plotlyOutput("plot_graph1", height = "460px")))
            )
    ),
    
    # create data table layer 
    tabItem("table",
            inputPanel(
              
              # add button to download table as csv
              downloadButton("downloadData","Download Use of Force Data") 
            ),
            fluidPage(
              box(title = "Selected Crime Stats", DT::dataTableOutput("table"), width = 24))
    )
  ))


ui <- dashboardPage(header, sidebar, body)

# define server logic
server <- function(input, output, session = session) {
  forceInput <- reactive({
    
    # neighborhood api code
    neighb_input <- if (length(input$neighbSelect) == 0) {""} else {
      paste0("AND (sna_neighborhood= '",
             paste0(
               paste0(input$neighbSelect, collapse = "' OR sna_neighborhood= '"),
               "')"))}
    
    # incident api code
    type_input <- if (length(input$incSelect) == 0) {""} else {
      paste0("AND (incident_description= '",
             paste0(
               paste0(input$incSelect, collapse = "' OR incident_description= '"),
               "')"))}
    
    # officer race api code
    oRace_input <- if (length(input$offRaceSelect) == 0) {""} else {
      paste0("AND (officer_race= '",
             paste0(
               paste0(input$offRaceSelect, collapse = "' OR officer_race= '"),
               "')"))}
    
    # officer gender api code
    oGender_input <- if (length(input$offGendSelect) == 0) {""} else {
      paste0("AND (officer_gender= '",
             paste0(
               paste0(input$offGendSelect, collapse = "' OR officer_gender= '"),
               "')"))}
    
    # subject race api code
    sRace_input <- if (length(input$susRaceSelect) == 0) {""} else {
      paste0("AND (subject_race= '",
             paste0(
               paste0(input$susRaceSelect, collapse = "' OR subject_race= '"),
               "')"))}
    
    # subject gender api code
    sGender_input <- if (length(input$susGendSelect) == 0) {""} else {
      paste0("AND (subject_gender= '",
             paste0(
               paste0(input$susGendSelect, collapse = "' OR subject_gender= '"),
               "')"))}
    
    # read in data
    forceInput <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                      input$dateSelect[1], "T00:00:00' AND incident_date <= '", input$dateSelect[2], "T23:59:59' ", 
                                      neighb_input, 
                                      type_input, 
                                      oRace_input, 
                                      oGender_input, 
                                      sRace_input, 
                                      sGender_input,""),
                               app_token = token)
    
    # clear nas
    forceInput <- na.omit(forceInput)
    
    # error message
    if(nrow(forceInput)==0){
      shinyalert("Oops!", "There are no incidents matching your search. Please try again.", type = "error")
      forceInput <- NULL
    }
    forceInput
  })
  
  
  # plot map
  output$plot_map <- renderLeaflet ({
    
    # subset shapefile by neighborhood
    if (length(input$neighbSelect) != 0) {
      cinciNeighb@data$SNA_NAME <- toupper(cinciNeighb@data$SNA_NAME)
      cinciNeighb <- subset(cinciNeighb, SNA_NAME %in% input$neighbSelect)
    }
    
    # create awesome icons      
    icons <- reactive({
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = forceInput()$levelColor
      )
      
      return(icons)
    })
    
    # input basemap   
    if(length(forceInput()) >= 1) {
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik", 
                         group = "Street", 
                         options = providerTileOptions(minZoom=11, maxZoom=30)) %>%
        addPolygons(data = cinciNeighb, 
                    weight = 1.5, 
                    color = "black") %>%
        addAwesomeMarkers(data = forceInput(), 
                          lng = ~as.numeric(longitude_x), 
                          lat = ~as.numeric(latitude_x),
                          popup = ~paste0("<b>Neighborhood: </b>", str_to_title(sna_neighborhood), "<br>",
                                          "<b>Incident Date: </b>", substr(incident_date, start = 1, stop = 10), "<br>",
                                          "<b>Incident Time: </b>", substr(incident_date, start = 12, stop = 19), "<br>",
                                          "<b>Officer Race: </b>", str_to_title(officer_race), "<br>",
                                          "<b>Officer Gender: </b>", str_to_title(officer_gender), "<br>",
                                          "<b>Subject Race: </b>", str_to_title(subject_race), "<br>",
                                          "<b>Subject Gender: </b>", str_to_title(subject_gender), "<br>"),
                          icon=icons(),
                          clusterOptions = markerClusterOptions()) %>%
        setView(lng = -84.51, lat = 39.15, zoom = 11) %>%
        setMaxBounds(lng1 = -84.74, lat1 = 39.23, lng2 = -84.34, lat2 = 39.04)
    }
  })
  
  # plot histogram
  output$plot_graph1 <- renderPlotly({
    if(length(forceInput()) >= 1) {
      ggplotly(
        ggplot(data = forceInput(), aes(x = sna_neighborhood,
                                        text = paste0("<b>Total: </b>", ..count.. %>% round(digits = 0)))) +
          aes_string(fill = input$fillSelect) +
          geom_histogram(stat = "count") +
          labs(y = "Total Incidents",
               title = "Police Use of Force by Neighborhood & Demographics Characteristics",
               x = NULL) +
          theme(plot.title = element_text(family = 'Helvetica',  
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 18, 
                                          hjust = 0)) +
          theme(axis.title.y = element_text(family = 'Helvetica', 
                                            color = '#181414', 
                                            face = 'bold', 
                                            size = 12, 
                                            hjust = 0)) +
          theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
          scale_y_continuous(labels = comma) +
          guides(color = FALSE)
        , tooltip = "text") %>% 
        layout(legend = list(title=list(text=paste0("<b>", str_replace_all(input$fillSelect, "_", " ") %>% 
                                                      str_to_title, "</b>")),
                             orientation = "h",  # use center of legend as anchor
                             x = 0, y = -1.5))
    }
  })
  
  # render crime datatable
  output$table <- DT::renderDataTable({
    subset(forceInput(), select = colnames(forceInput()))
  },
  options = list(
    autoWidth = TRUE,
    scrollX = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all"))
  ))
  
  # url bar update
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # make data table downloadable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cincinnati-force-data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(forceInput(), file)
    }
  )
  
  # reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "neighbSelect", selected = c(""))
    updateSelectInput(session, "incSelect", selected = c(""))
    updateSelectInput(session, "offGendSelect", selected = "ALL")
    updateSelectInput(session, "offRaceSelect", selected = "ALL")
    updateSelectInput(session, "susGendSelect", selected = "ALL")
    updateSelectInput(session, "susRaceSelect", selected = "ALL")
    updateDateRangeInput(session, "dateSelect", start = Sys.Date()-365, end = dateMax)
    showNotification("You have reset the filters", 
                     type = "message", 
                     duration = 3, 
                     closeButton = F)
  })
  observe({
    input$reset_button
    leafletProxy("plot_map") %>% setView(lng = -84.51, lat = 39.15, zoom = 11)
  })
}

# run the application 
shinyApp(ui = ui, server = server)