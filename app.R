# Jonathon Repta
# CS 424 Project 3

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(mapview)
library(tigris)
library(leaflet)

# Read in compressed data
energy <- read.csv(file = "data/Energy_Usage_2010_.csv", sep = ",", header=TRUE)

# Get blocks for Cook county
blocks <- blocks(state = 'IL', county = 'Cook')

# Get only the blocks we need
chicago_blocks <- subset(blocks, GEOID10 %in% energy$census_block)

# Get NWS
nws <- subset(energy, area_name == "Near West Side")

# Get NWS blocks
nws_blocks <- subset(blocks, GEOID10 %in% nws$census_block)


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
              c("Albany Park",
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
              column(6,selectInput("monthSelect1", "Month", c("January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="Jan", width="100%"))
            ),
            mapviewOutput("areaMap1", height="calc(100vh - 245px)"),
            absolutePanel(top = 90, left=25, actionButton("resetButton1", "Reset Map")),
            selectInput(inputId="typeSelect1", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas"), selected="kwh")
          ),
          column(6,
            fluidRow(column(6,
              selectInput(inputId="areaSelect2", width="100%", label="Community area", choices=
              c("Albany Park",
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
              column(6,selectInput("monthSelect2", "Month", c("January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="Jan", width="100%"))
            ),
            mapviewOutput("areaMap2", height="calc(100vh - 245px)"),
            absolutePanel(top = 90, left=25, actionButton("resetButton2", "Reset Map")),
            selectInput(inputId="typeSelect2", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas"), selected="kwh")
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
  determine <- function(inputVal) {
    zcol <- "kwh_total"

    if (input$monthSelect1 == "Jan") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_january"
        }
        else {
            zcol <- "kwh_january"
        }
    }
    else if (input$monthSelect1 == "Feb") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_february"
        }
        else {
            zcol <- "kwh_february"
        }
    }
    else if (input$monthSelect1 == "Mar") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_march"
        }
        else {
            zcol <- "kwh_march"
        }
    }
    else if (input$monthSelect1 == "Apr") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_april"
        }
        else {
            zcol <- "kwh_april"
        }
    }
    else if (input$monthSelect1 == "May") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_may"
        }
        else {
            zcol <- "kwh_may"
        }
    }
    else if (input$monthSelect1 == "Jun") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_june"
        }
        else {
            zcol <- "kwh_june"
        }
    }
    else if (input$monthSelect1 == "Jul") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_july"
        }
        else {
            zcol <- "kwh_july"
        }
    }
    else if (input$monthSelect1 == "Aug") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_august"
        }
        else {
            zcol <- "kwh_august"
        }
    }
    else if (input$monthSelect1 == "Sep") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_september"
        }
        else {
            zcol <- "kwh_september"
        }
    }
    else if (input$monthSelect1 == "Oct") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_october"
        }
        else {
            zcol <- "kwh_october"
        }
    }
    else if (input$monthSelect1 == "Nov") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_november"
        }
        else {
            zcol <- "kwh_november"
        }
    }
    else if (input$monthSelect1 == "Dec") {
        if (input$typeSelect1 == "gas") {
            zcol <- "therm_december"
        }
        else {
            zcol <- "khw_december"
        }
    }

    zcol
  }

  activeArea1 <- reactive({
    toReturn <- NULL

    active_energy_area <- subset(energy, area_name == input$areaSelect1)
    active_blocks <- subset(blocks, GEOID10 %in% active_energy_area$census_block)

    toReturn <- merge(x=active_blocks, y=active_energy_area, by.x=c("GEOID10"), by.y=c("census_block"), all.y=TRUE)

    mapview(toReturn, zcol = determine())
  })

  activeArea2 <- reactive({
    toReturn <- NULL

    active_energy_area <- subset(energy, area_name == input$areaSelect2)
    active_blocks <- subset(blocks, GEOID10 %in% active_energy_area$census_block)

    toReturn <- merge(x=active_blocks, y=active_energy_area, by.x=c("GEOID10"), by.y=c("census_block"), all.y=TRUE)

    mapview(toReturn, zcol = "therm_toal")
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
