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
              column(6,selectInput("monthSelect1", "Month", c("All","January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="All", width="100%"))
            ),
            fluidRow(column(6, selectInput(inputId="buildingSelect1", width="100%", label="Building type", choices=c("All", "Commercial", "Industrial", "Residential"), selected="All")),
                     column(6, selectInput(inputId="typeSelect1", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas", "Population" = "pop", "Building height" = "height", "Building size" = "size", "Building age" = "age"), selected="kwh"))),
            mapviewOutput("areaMap1", height="calc(100vh - 245px)"),
            absolutePanel(top = 90, left=25, actionButton("resetButton1", "Reset Map")),
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
              column(6,selectInput("monthSelect2", "Month", c("All","January" = "Jan","February" = "Feb","March" = "Mar","April" = "Apr","May" = "May","June" = "Jun","July" = "Jul","August" = "Aug","September" = "Sep","October" = "Oct","November" = "Nov","December" = "Dec"), selected="All", width="100%"))
            ),
            fluidRow(column(6, selectInput(inputId="buildingSelect2", width="100%", label="Building type", choices=c("All", "Commercial", "Industrial", "Residential"), selected="All")),
                     column(6, selectInput(inputId="typeSelect2", width="100%", label="Source", choices=c("Energy" = "kwh", "Gas" = "gas", "Population" = "pop", "Building height" = "height", "Building size" = "size", "Building age" = "age"), selected="kwh"))),
            mapviewOutput("areaMap2", height="calc(100vh - 245px)"),
            absolutePanel(top = 180, left=25, actionButton("resetButton2", "Reset Map"))
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
    monthSelect <- input$monthSelect1
    typeSelect <- input$typeSelect1

    if (inputVal == 1) {
      monthSelect <- input$monthSelect1
      typeSelect <- input$typeSelect1
    }
    else {
      monthSelect <- input$monthSelect2
      typeSelect <- input$typeSelect2
    }

    zcol <- "kwh_total"

    if (monthSelect == "Jan") {
        if (typeSelect == "gas") {
            zcol <- "therm_january"
        }
        else {
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
  })

  activeArea1 <- reactive({
    toReturn <- NULL

    active_energy_area <- subset(energy, area_name == input$areaSelect1)

    if (input$buildingSelect1 != "All") {
      active_energy_area <- subset(active_energy_area, building_type == input$buildingSelect1)
    }

    active_blocks <- subset(blocks, GEOID10 %in% active_energy_area$census_block)

    toReturn <- merge(x=active_blocks, y=active_energy_area, by.x=c("GEOID10"), by.y=c("census_block"), all.y=TRUE)

    mapview(toReturn, zcol = determine_zcol(1))
  })

  activeArea2 <- reactive({
    toReturn <- NULL

    active_energy_area <- subset(energy, area_name == input$areaSelect2)

    if (input$buildingSelect2 != "All") {
      active_energy_area <- subset(active_energy_area, building_type == input$buildingSelect2)
    }

    active_blocks <- subset(blocks, GEOID10 %in% active_energy_area$census_block)

    toReturn <- merge(x=active_blocks, y=active_energy_area, by.x=c("GEOID10"), by.y=c("census_block"), all.y=TRUE)

    mapview(toReturn, zcol = determine_zcol(2))
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
