# Jonathon Repta
# CS 424 Project 3

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(mapview)
library(tigris)
library(leaflet)
library(RColorBrewer)
library(dplyr)

# Read in compressed data
energy <- read.csv(file = "data/Energy_Usage_2010_Cleaned.csv", sep = ",", header=TRUE)

# Get blocks/tracts for Cook county
blocks <- blocks(state = 'IL', county = 'Cook')
tracts <- tracts(state = 'IL', county = 'Cook')

# Get tracts/blocks we have data for (Chicago)
chicago_blocks <- subset(blocks, GEOID10 %in% energy$census_block)
chicago_tracts <- subset(tracts, TRACTCE %in% chicago_blocks$TRACTCE10)

tract_lookup <- chicago_blocks

# Remove unecessary columns
tract_lookup$STATEFP10 <- NULL
tract_lookup$COUNTYFP10 <- NULL
tract_lookup$BLOCKCE10 <- NULL
tract_lookup$NAME10 <- NULL
tract_lookup$MTFCC10 <- NULL
tract_lookup$UR10 <- NULL
tract_lookup$UACE10 <- NULL
tract_lookup$UATYPE <- NULL
tract_lookup$FUNCSTAT10 <- NULL
tract_lookup$ALAND10 <- NULL
tract_lookup$AWATER10 <- NULL
tract_lookup$INTPTLAT10 <- NULL
tract_lookup$INTPTLON10 <- NULL
tract_lookup$geometry <- NULL
tract_lookup$COUNTYFP <- NULL
tract_lookup$STATEFP <- NULL

# Add tract data to energy dataframe
energy <- merge(x=tract_lookup, y=energy, by.x=c("GEOID10"), by.y=c("census_block"), all.y=TRUE)

ui <- fluidPage(
    title = "CS 424: Project 3",
    lang="en",
    p(),
    navbarPage("Project 3", position = c("static-top"), collapsible = FALSE, fluid = TRUE,
    tabPanel("Compare community areas",
    fluidPage(
          column(6,
            fluidRow(column(6,
              selectInput(inputId="areaSelect1", width="100%", label="Community area", choices=
              c("All",
              "Albany Park",
              "Archer Heights",
              "Armour Square",
              "Ashburn",
              "Auburn Gresham",
              "Austin",
              "Avalon Park",
              "Avondale",
              "Belmont Cragin",
              "Beverly",
              "Bridgeport",
              "Brighton Park",
              "Burnside",
              "Calumet Heights",
              "Chatham",
              "Chicago Lawn",
              "Clearing",
              "Douglas",
              "Dunning",
              "East Garfield Park",
              "East Side",
              "Edgewater",
              "Edison Park",
              "Englewood",
              "Forest Glen",
              "Fuller Park",
              "Gage Park",
              "Garfield Ridge",
              "Grand Boulevard",
              "Greater Grand Crossing",
              "Hegewisch",
              "Hermosa",
              "Humboldt Park",
              "Hyde Park",
              "Irving Park",
              "Jefferson Park",
              "Kenwood",
              "Lakeview",
              "Lincoln Park",
              "Lincoln Square",
              "Logan Square",
              "Loop",
              "Lower West Side",
              "McKinley Park",
              "Montclare",
              "Morgan Park",
              "Mount Greenwood",
              "Near North Side",
              "Near South Side",
              "Near West Side",
              "New City",
              "North Center",
              "North Lawndale",
              "North Park",
              "Norwood Park",
              "O'Hare",
              "Oakland",
              "Portage Park",
              "Pullman",
              "Riverdale",
              "Rogers Park",
              "Roseland",
              "South Chicago",
              "South Deering",
              "South Lawndale",
              "South Shore",
              "Uptown",
              "Washington Heights",
              "Washington Park",
              "West Elsdon",
              "West Englewood",
              "West Garfield Park",
              "West Lawn",
              "West Pullman",
              "West Ridge",
              "West Town",
              "Woodlawn"),
              selected="Near West Side")),
              column(6,selectInput("monthSelect1", "Month", c("All","January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="All", width="100%"))
            ),
            fluidRow(column(6, selectInput(inputId="buildingSelect1", width="100%", label="Building type", choices=c("All", "Commercial", "Industrial", "Residential"), selected="All")),
                     column(6, selectInput(inputId="typeSelect1", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas", "Population" = "pop", "Building height" = "height", "Building size" = "size", "Building age" = "age"), selected="kwh"))),
          ),
          column(6,
            fluidRow(column(6,
              selectInput(inputId="areaSelect2", width="100%", label="Community area", choices=
              c("All",
              "Albany Park",
              "Archer Heights",
              "Armour Square",
              "Ashburn",
              "Auburn Gresham",
              "Austin",
              "Avalon Park",
              "Avondale",
              "Belmont Cragin",
              "Beverly",
              "Bridgeport",
              "Brighton Park",
              "Burnside",
              "Calumet Heights",
              "Chatham",
              "Chicago Lawn",
              "Clearing",
              "Douglas",
              "Dunning",
              "East Garfield Park",
              "East Side",
              "Edgewater",
              "Edison Park",
              "Englewood",
              "Forest Glen",
              "Fuller Park",
              "Gage Park",
              "Garfield Ridge",
              "Grand Boulevard",
              "Greater Grand Crossing",
              "Hegewisch",
              "Hermosa",
              "Humboldt Park",
              "Hyde Park",
              "Irving Park",
              "Jefferson Park",
              "Kenwood",
              "Lakeview",
              "Lincoln Park",
              "Lincoln Square",
              "Logan Square",
              "Loop",
              "Lower West Side",
              "McKinley Park",
              "Montclare",
              "Morgan Park",
              "Mount Greenwood",
              "Near North Side",
              "Near South Side",
              "Near West Side",
              "New City",
              "North Center",
              "North Lawndale",
              "North Park",
              "Norwood Park",
              "O'Hare",
              "Oakland",
              "Portage Park",
              "Pullman",
              "Riverdale",
              "Rogers Park",
              "Roseland",
              "South Chicago",
              "South Deering",
              "South Lawndale",
              "South Shore",
              "Uptown",
              "Washington Heights",
              "Washington Park",
              "West Elsdon",
              "West Englewood",
              "West Garfield Park",
              "West Lawn",
              "West Pullman",
              "West Ridge",
              "West Town",
              "Woodlawn"),
              selected="Loop")),
              column(6,selectInput("monthSelect2", "Month", c("All","January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="All", width="100%"))
            ),
            fluidRow(column(6, selectInput(inputId="buildingSelect2", width="100%", label="Building type", choices=c("All", "Commercial", "Industrial", "Residential"), selected="All")),
                     column(6, selectInput(inputId="typeSelect2", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas", "Population" = "pop", "Building height" = "height", "Building size" = "size", "Building age" = "age"), selected="kwh"))),
          ),
          tabsetPanel(
            id = 'dataset',
            tabPanel("Maps",
              fluidRow(
                column(6,
                  mapviewOutput("areaMap1", height="70vh"),
                  absolutePanel(bottom = 120, left = 25, actionButton("resetButton1", "Reset Map")),
                  checkboxInput("checkbox1", "Census tract", value = FALSE)
                ),
                column(6,
                  mapviewOutput("areaMap2", height="70vh"),
                  absolutePanel(bottom = 120, left = 25, actionButton("resetButton2", "Reset Map")),
                  checkboxInput("checkbox2", "Census tract", value = FALSE)
                )
              )
            ),
            tabPanel("Bar charts",
              fluidRow(
                column(6,
                        plotOutput("bar0"),
                        plotOutput("bar3")
                ),
                column(6,
                        plotOutput("bar1"),
                        plotOutput("bar4")
                )
              )
            ),
            tabPanel("Datatable",
              fluidRow(
                column(6,
                  DT::dataTableOutput("table0"),
                ),
                column(6,
                  DT::dataTableOutput("table1"),
                )
                )
              )
            )
        )
    ),
  tabPanel("About",
    verbatimTextOutput("name"),
    verbatimTextOutput("date"),
    verbatimTextOutput("dataset")
  )
))

server <- function(input, output, session) {
  determine_zcol <- function(inputVal) {
    monthSelect <- NULL
    typeSelect <- NULL

    if (inputVal == 1) {
      monthSelect <- input$monthSelect1
      typeSelect <- input$typeSelect1
    }
    else if (inputVal == 2)  {
      monthSelect <- input$monthSelect2
      typeSelect <- input$typeSelect2
    }

    if (typeSelect == "gas") {
      zcol <- "therm_total"
    }
    else {
      zcol <- "kwh_total"
    }

    if (monthSelect == "Jan") {
        if (typeSelect == "gas") {
            zcol <- "therm_january"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_january"
        }
    }
    else if (monthSelect == "Feb") {
        if (typeSelect == "gas") {
            zcol <- "therm_february"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_february"
        }
    }
    else if (monthSelect == "Mar") {
        if (typeSelect == "gas") {
            zcol <- "therm_march"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_march"
        }
    }
    else if (monthSelect == "Apr") {
        if (typeSelect == "gas") {
            zcol <- "therm_april"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_april"
        }
    }
    else if (monthSelect == "May") {
        if (typeSelect == "gas") {
            zcol <- "therm_may"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_may"
        }
    }
    else if (monthSelect == "Jun") {
        if (typeSelect == "gas") {
            zcol <- "therm_june"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_june"
        }
    }
    else if (monthSelect == "Jul") {
        if (typeSelect == "gas") {
            zcol <- "therm_july"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_july"
        }
    }
    else if (monthSelect == "Aug") {
        if (typeSelect == "gas") {
            zcol <- "therm_august"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_august"
        }
    }
    else if (monthSelect == "Sep") {
        if (typeSelect == "gas") {
            zcol <- "therm_september"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_september"
        }
    }
    else if (monthSelect == "Oct") {
        if (typeSelect == "gas") {
            zcol <- "therm_october"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_october"
        }
    }
    else if (monthSelect == "Nov") {
        if (typeSelect == "gas") {
            zcol <- "therm_november"
        }
        else if (typeSelect == "kwh") {
            zcol <- "kwh_november"
        }
    }
    else if (monthSelect == "Dec") {
        if (typeSelect == "gas") {
            zcol <- "therm_december"
        }
        else if (typeSelect == "kwh") {
            zcol <- "khw_december"
        }
    }

    if (typeSelect == "age") {
      zcol <- "building_age"
    }
    else if (typeSelect == "height") {
      zcol <- "building_height"
    }
    else if (typeSelect == "size") {
      zcol <- "building_size"
    }
    else if (typeSelect == "pop") {
      zcol <- "total_population"
    }

    zcol
  }

  observeEvent(input$resetButton1, {
    updateSelectInput(session, "areaSelect1",
      selected = "Near West Side"
    )
    updateSelectInput(session, "monthSelect1",
      selected = "All"
    )
    updateSelectInput(session, "buildingSelect1",
      selected = "All"
    )
    updateSelectInput(session, "typeSelect1",
      selected = "kwh"
    )
    updateCheckboxInput(session, "checkbox1", value = FALSE)
  })

  observeEvent(input$resetButton2, {
    updateSelectInput(session, "areaSelect2",
      selected = "Loop"
    )
    updateSelectInput(session, "monthSelect2",
      selected = "All"
    )
    updateSelectInput(session, "buildingSelect2",
      selected = "All"
    )
    updateSelectInput(session, "typeSelect2",
      selected = "kwh"
    )
    updateCheckboxInput(session, "checkbox2", value = FALSE)
  })

  filterBlocks <- function(inputVal) {
    toReturn <- NULL
    active_energy_area <- NULL
    active_blocks <- NULL

    monthSelect <- NULL
    typeSelect <- NULL
    areaSelect <- NULL
    buildingSelect <- NULL

    # Select correct inputs
    if (inputVal == 1) {
      monthSelect <- input$monthSelect1
      typeSelect <- input$typeSelect1
      areaSelect <- input$areaSelect1
      buildingSelect <- input$buildingSelect1
    }
    else if (inputVal == 2) {
      monthSelect <- input$monthSelect2
      typeSelect <- input$typeSelect2
      areaSelect <- input$areaSelect2
      buildingSelect <- input$buildingSelect2
    }

    # Filter energy by area
    if (areaSelect != "All") {
      active_energy_area <- subset(energy, area_name == areaSelect)
    }
    else {
      active_energy_area <- energy
    }

    if (buildingSelect != "All") {
      active_energy_area <- subset(active_energy_area, building_type == buildingSelect)
    }

    active_blocks <- subset(blocks, GEOID10 %in% active_energy_area$GEOID10)

    # Tract mode
    if (inputVal == 1 && input$checkbox1 == TRUE) {
      toReturn <- aggregateData(determine_zcol(1), active_energy_area)
    }
    else if (inputVal == 2 && input$checkbox2 == TRUE) {
      toReturn <- aggregateData(determine_zcol(2), active_energy_area)
    }
    else {
      toReturn <- merge(x=active_blocks, y=active_energy_area, by.x=c("GEOID10"), by.y=c("GEOID10"), all.y=TRUE)
    }

    toReturn
  }

  aggregateData <- function(aggr_by, activeArea) {

    # Remove non-aggregateable columns
    activeArea$GEOID10 <- NULL
    activeArea$area_name <- NULL
    activeArea$building_type <- NULL

    group <- NULL

    if (aggr_by == "kwh_total") {
      group <- aggregate(kwh_total ~ TRACTCE10, data = activeArea, sum)
    }
    else if (aggr_by == "therm_total") {
      group <- aggregate(therm_total ~ TRACTCE10, data = activeArea, sum)
    }
    else if (aggr_by == "total_population") {
      group <- aggregate(total_population ~ TRACTCE10, data = activeArea, sum)
    }
    else if (aggr_by == "building_height") {
      group <- aggregate(building_height ~ TRACTCE10, data = activeArea, mean)
    }
    else if (aggr_by == "building_size"){
      group <- aggregate(building_size ~ TRACTCE10, data = activeArea, mean)
    }
    else if (aggr_by == "building_age") {
      group <- aggregate(building_age ~ TRACTCE10, data = activeArea, mean)
    }

    aggr_data <- merge(x=chicago_tracts, y=group, by.x=c("TRACTCE"), by.y=c("TRACTCE10"), all.y=TRUE)
    aggr_data
  }

  formatName <- function(inputVal) {
    # Select correct inputs
    if (inputVal == 1) {
      monthSelect <- input$monthSelect1
      typeSelect <- input$typeSelect1
      areaSelect <- input$areaSelect1
      buildingSelect <- input$buildingSelect1
    }
    else if (inputVal == 2) {
      monthSelect <- input$monthSelect2
      typeSelect <- input$typeSelect2
      areaSelect <- input$areaSelect2
      buildingSelect <- input$buildingSelect2
    }

    label <- NULL

    if (typeSelect == "kwh") {
      label <- "Energy usage"
    }
    else if (typeSelect == "gas") {
      label <- "Gas usage"
    }
    else if (typeSelect == "age") {
      label <- "Building age"
    }
    else if (typeSelect == "height") {
      label <- "Building height"
    }
    else if (typeSelect == "size") {
      label <- "Building size"
    }
    else if (typeSelect == "pop") {
      label <- "Building population"
    }

    if (inputVal == 1) {
      formattedName <- stringr::str_interp("${label}.")
    }
    else {
      formattedName <- stringr::str_interp("${label}")
    }

    formattedName
  }

  activeArea1 <- reactive({
    toReturn <- filterBlocks(1)
    mapview(toReturn, zcol = determine_zcol(1), layer.name=formatName(1), basemaps=c("CartoDB.Positron","CartoDB.DarkMatter","OpenStreetMap", "Esri.WorldImagery","OpenTopoMap"))
  })

  activeArea2 <- reactive({
    toReturn <- filterBlocks(2)
    mapview(toReturn, zcol = determine_zcol(2), layer.name=formatName(2), basemaps=c("CartoDB.Positron","CartoDB.DarkMatter","OpenStreetMap", "Esri.WorldImagery","OpenTopoMap"))
  })

  plotData0 <- reactive({
    active <- filterBlocks(1)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    kwh <- c(sum(active$kwh_january, na.rm = TRUE),
             sum(active$kwh_february, na.rm = TRUE),
             sum(active$kwh_march, na.rm = TRUE),
             sum(active$kwh_april, na.rm = TRUE),
             sum(active$kwh_may, na.rm = TRUE),
             sum(active$kwh_june, na.rm = TRUE),
             sum(active$kwh_july, na.rm = TRUE),
             sum(active$kwh_august, na.rm = TRUE),
             sum(active$kwh_september, na.rm = TRUE),
             sum(active$kwh_october, na.rm = TRUE),
             sum(active$kwh_november, na.rm = TRUE),
             sum(active$khw_december, na.rm = TRUE))

    toReturn <- data.frame(months, kwh)

    toReturn
  })

  plotData1 <- reactive({
    active <- filterBlocks(2)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    kwh <- c(sum(active$kwh_january, na.rm = TRUE),
             sum(active$kwh_february, na.rm = TRUE),
             sum(active$kwh_march, na.rm = TRUE),
             sum(active$kwh_april, na.rm = TRUE),
             sum(active$kwh_may, na.rm = TRUE),
             sum(active$kwh_june, na.rm = TRUE),
             sum(active$kwh_july, na.rm = TRUE),
             sum(active$kwh_august, na.rm = TRUE),
             sum(active$kwh_september, na.rm = TRUE),
             sum(active$kwh_october, na.rm = TRUE),
             sum(active$kwh_november, na.rm = TRUE),
             sum(active$khw_december, na.rm = TRUE))

    toReturn <- data.frame(months, kwh)

    toReturn
  })

  plotData3 <- reactive({
    active <- filterBlocks(1)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    therm <- c(sum(active$therm_january, na.rm = TRUE),
               sum(active$therm_february, na.rm = TRUE),
               sum(active$therm_march, na.rm = TRUE),
               sum(active$therm_april, na.rm = TRUE),
               sum(active$therm_may, na.rm = TRUE),
               sum(active$therm_june, na.rm = TRUE),
               sum(active$therm_july, na.rm = TRUE),
               sum(active$therm_august, na.rm = TRUE),
               sum(active$therm_september, na.rm = TRUE),
               sum(active$therm_october, na.rm = TRUE),
               sum(active$therm_november, na.rm = TRUE),
               sum(active$therm_december, na.rm = TRUE))

    toReturn <- data.frame(months, therm)

    toReturn
  })

  plotData4 <- reactive({
    active <- filterBlocks(2)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    therm <- c(sum(active$therm_january, na.rm = TRUE),
               sum(active$therm_february, na.rm = TRUE),
               sum(active$therm_march, na.rm = TRUE),
               sum(active$therm_april, na.rm = TRUE),
               sum(active$therm_may, na.rm = TRUE),
               sum(active$therm_june, na.rm = TRUE),
               sum(active$therm_july, na.rm = TRUE),
               sum(active$therm_august, na.rm = TRUE),
               sum(active$therm_september, na.rm = TRUE),
               sum(active$therm_october, na.rm = TRUE),
               sum(active$therm_november, na.rm = TRUE),
               sum(active$therm_december, na.rm = TRUE))

    toReturn <- data.frame(months, therm)

    toReturn
  })

  # Bar charts
  output$bar0 <- renderPlot({
    ggplot(data=plotData0(), aes(x = factor(months, levels=(months)), y = kwh)) +
    geom_bar(stat="identity", fill="#4FC3F7", colour="#01579B")+
    labs(title="Monthly Energy usage", x = "Month", y = "Energy used (kWh)")+
    scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
  })

  output$bar1 <- renderPlot({
    ggplot(data=plotData1(), aes(x = factor(months, levels=(months)), y = kwh)) +
    geom_bar(stat="identity", fill="#4FC3F7", colour="#01579B")+
    labs(title="Monthly Energy usage", x = "Month", y = "Energy used (kWh)")+
    scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
  })

  output$bar3 <- renderPlot({
    ggplot(data=plotData3(), aes(x = factor(months, levels=(months)), y = therm)) +
    geom_bar(stat="identity", fill="#FFB74D", colour="#E65100")+
    labs(title="Monthly gas usage", x = "Month", y = "Gas used")+
    scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
  })

  output$bar4 <- renderPlot({
    ggplot(data=plotData4(), aes(x = factor(months, levels=(months)), y = therm)) +
    geom_bar(stat="identity", fill="#FFB74D", colour="#E65100")+
    labs(title="Monthly gas usage", x = "Month", y = "Gas used")+
    scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
  })

  # Mapviews
  output$areaMap1 <- renderLeaflet({
    m <- activeArea1()
    m@map
  })

  output$areaMap2 <- renderLeaflet({
    m2 <- activeArea2()
    m2@map
  })

  output$table0 <- DT::renderDataTable({
    active <- filterBlocks(1)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    kwh <- c(sum(active$kwh_january, na.rm = TRUE),
             sum(active$kwh_february, na.rm = TRUE),
             sum(active$kwh_march, na.rm = TRUE),
             sum(active$kwh_april, na.rm = TRUE),
             sum(active$kwh_may, na.rm = TRUE),
             sum(active$kwh_june, na.rm = TRUE),
             sum(active$kwh_july, na.rm = TRUE),
             sum(active$kwh_august, na.rm = TRUE),
             sum(active$kwh_september, na.rm = TRUE),
             sum(active$kwh_october, na.rm = TRUE),
             sum(active$kwh_november, na.rm = TRUE),
             sum(active$khw_december, na.rm = TRUE))

    therm <- c(sum(active$therm_january, na.rm = TRUE),
               sum(active$therm_february, na.rm = TRUE),
               sum(active$therm_march, na.rm = TRUE),
               sum(active$therm_april, na.rm = TRUE),
               sum(active$therm_may, na.rm = TRUE),
               sum(active$therm_june, na.rm = TRUE),
               sum(active$therm_july, na.rm = TRUE),
               sum(active$therm_august, na.rm = TRUE),
               sum(active$therm_september, na.rm = TRUE),
               sum(active$therm_october, na.rm = TRUE),
               sum(active$therm_november, na.rm = TRUE),
               sum(active$therm_december, na.rm = TRUE))

    data <- data.frame(months, format(therm, big.mark=",", scientific = FALSE), format(kwh, big.mark=",", scientific = FALSE))

    names(data) <- c("Month", "Gas usage", "Electrical usage (kWh)")
    DT::datatable(data, options = list(orderClasses = TRUE, pageLength = 100))
  })

  output$table1 <- DT::renderDataTable({
    active <- filterBlocks(2)
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December")

    kwh <- c(sum(active$kwh_january, na.rm = TRUE),
             sum(active$kwh_february, na.rm = TRUE),
             sum(active$kwh_march, na.rm = TRUE),
             sum(active$kwh_april, na.rm = TRUE),
             sum(active$kwh_may, na.rm = TRUE),
             sum(active$kwh_june, na.rm = TRUE),
             sum(active$kwh_july, na.rm = TRUE),
             sum(active$kwh_august, na.rm = TRUE),
             sum(active$kwh_september, na.rm = TRUE),
             sum(active$kwh_october, na.rm = TRUE),
             sum(active$kwh_november, na.rm = TRUE),
             sum(active$khw_december, na.rm = TRUE))

    therm <- c(sum(active$therm_january, na.rm = TRUE),
               sum(active$therm_february, na.rm = TRUE),
               sum(active$therm_march, na.rm = TRUE),
               sum(active$therm_april, na.rm = TRUE),
               sum(active$therm_may, na.rm = TRUE),
               sum(active$therm_june, na.rm = TRUE),
               sum(active$therm_july, na.rm = TRUE),
               sum(active$therm_august, na.rm = TRUE),
               sum(active$therm_september, na.rm = TRUE),
               sum(active$therm_october, na.rm = TRUE),
               sum(active$therm_november, na.rm = TRUE),
               sum(active$therm_december, na.rm = TRUE))


    data <- data.frame(months, format(therm, big.mark=",", scientific = FALSE), format(kwh, big.mark=",", scientific = FALSE))

    names(data) <- c("Month", "Gas usage", "Electrical usage (kWh)")
    DT::datatable(data, options = list(orderClasses = TRUE, pageLength = 100))
  })

  # About page
  output$name <- renderPrint({
    "Created by: Jonathon Repta"
  })
  output$date <- renderPrint({
    "Created on: April 24, 2021"
  })
  output$dataset <- renderPrint({
    "Data from: https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp, Energy Usage 2010"
  })
}

shinyApp(ui, server)
