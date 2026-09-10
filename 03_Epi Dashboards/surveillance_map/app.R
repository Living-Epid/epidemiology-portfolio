





# Step 1: Install/load required packages for Project 3

packages <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "lubridate",
  "scales",
  "leaflet",         # interactive maps - the core new tool this project
  "leaflet.extras", # marker clustering, heatmaps
  "sf"             # spatial data handling (points, polygons)
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)
# Explicit library() calls - needed for Connect Cloud to detect dependencies via writeManifest()
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(lubridate)
library(scales)
library(leaflet)
library(leaflet.extras)
library(sf)



# Step 2: Simulate cholera cases with lat/lon coordinates around a city center

set.seed(2024)

n_cases <- 300

# --- Define a city center to simulate around (arbitrary coordinates) ---
city_lat <- 6.5244   # Lagos-area latitude, as an example reference point
city_lon <- 3.3792

# --- Define 3 outbreak "hotspot" clusters + general scatter ---
# Real cholera outbreaks cluster around contaminated water points
hotspots <- data.frame(
  hotspot_id = c("Water Point A", "Water Point B", "Water Point C"),
  lat = c(city_lat + 0.015, city_lat - 0.020, city_lat + 0.005),
  lon = c(city_lon - 0.018, city_lon + 0.012, city_lon + 0.025),
  weight = c(0.5, 0.3, 0.2)  # Water Point A is the dominant source
)

# --- Assign each case to a hotspot cluster, then jitter around it ---
assigned_hotspot <- sample(1:3, n_cases, replace = TRUE, prob = hotspots$weight)

line_list <- data.frame(
  case_id = sprintf("CH-%04d", 1:n_cases),
  age = round(rgamma(n_cases, shape = 2, scale = 12)),
  sex = sample(c("Male", "Female"), n_cases, replace = TRUE, prob = c(0.48, 0.52)),
  nearest_water_point = hotspots$hotspot_id[assigned_hotspot],
  lat = hotspots$lat[assigned_hotspot] + rnorm(n_cases, mean = 0, sd = 0.006),
  lon = hotspots$lon[assigned_hotspot] + rnorm(n_cases, mean = 0, sd = 0.006)
)

# --- Simulate onset timeline (same approach as Project 1) ---
outbreak_start <- as.Date("2024-03-01")
day_weights <- dgamma(1:45, shape = 3, rate = 0.25)
onset_days <- sample(1:45, n_cases, replace = TRUE, prob = day_weights)

line_list$date_onset <- outbreak_start + onset_days
line_list$outcome <- sample(c("Recovered", "Died"), n_cases, replace = TRUE, prob = c(0.97, 0.03))

# --- Quick check ---
str(line_list)
head(line_list, 10)


# Simulate simplified zone polygons + population (WHO/CDC choropleth style)

library(sf)

zone_population <- data.frame(
  zone = c("Water Point A", "Water Point B", "Water Point C"),
  population = c(28000, 19500, 12000)
)

# Build simple square polygons around each hotspot as a stand-in for real zone shapefiles
make_zone_polygon <- function(center_lat, center_lon, size = 0.025) {
  st_polygon(list(matrix(c(
    center_lon - size, center_lat - size,
    center_lon + size, center_lat - size,
    center_lon + size, center_lat + size,
    center_lon - size, center_lat + size,
    center_lon - size, center_lat - size
  ), ncol = 2, byrow = TRUE)))
}

zone_shapes <- st_sf(
  zone = hotspots$hotspot_id,
  geometry = st_sfc(
    make_zone_polygon(hotspots$lat[1], hotspots$lon[1]),
    make_zone_polygon(hotspots$lat[2], hotspots$lon[2]),
    make_zone_polygon(hotspots$lat[3], hotspots$lon[3])
  ),
  crs = 4326
)

print(zone_shapes)



# Step 3: Define the UI with the leaflet map

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#005EB8",
  danger = "#D32F2F",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Cholera Surveillance Map",
  theme = my_theme,
  
  sidebar = sidebar(
    title = "Filters",
    selectInput(
      inputId = "water_point_filter",
      label = "Nearest Water Point",
      choices = c("All", unique(line_list$nearest_water_point)),
      selected = "All"
    ),
    dateRangeInput(
      inputId = "date_range",
      label = "Onset Date Range",
      start = min(line_list$date_onset),
      end = max(line_list$date_onset)
    ),
    downloadButton("download_data", "Download Filtered Data (CSV)", class = "btn-primary")   # ← moved here, comma added above
  ),                                                                                          # ← this now closes sidebar(...)
  
  layout_columns(
    value_box(title = "Total Cases", value = textOutput("total_cases"), theme = "primary"),
    value_box(title = "Deaths", value = textOutput("total_deaths"), theme = "danger")
  ),
  
  navset_card_tab(
    nav_panel(
      "Case Map (Clustered)",
      leafletOutput("case_map", height = "500px")
    ),
    nav_panel(
      "Case Line List",
      DTOutput("line_list_table")
    ),
    nav_panel(
      "Attack Rate Map (Choropleth)",
      leafletOutput("choropleth_map", height = "500px")
    )
  )
)



# Step 3b: Server logic for the map

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- line_list
    
    if (input$water_point_filter != "All") {
      data <- data[data$nearest_water_point == input$water_point_filter, ]
    }
    
    data <- data[data$date_onset >= input$date_range[1] &
                   data$date_onset <= input$date_range[2], ]
    
    data
  })
  
  output$total_cases <- renderText({ nrow(filtered_data()) })
  output$total_deaths <- renderText({ sum(filtered_data()$outcome == "Died") })
  
  output$case_map <- renderLeaflet({
    df <- filtered_data()
    
    leaflet(df) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5,
        color = ~ifelse(outcome == "Died", "#D32F2F", "#005EB8"),
        fillOpacity = 0.7,
        stroke = FALSE,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", case_id, "</b><br>",
          "Age: ", age, "<br>",
          "Onset: ", date_onset, "<br>",
          "Nearest source: ", nearest_water_point, "<br>",
          "Outcome: ", outcome
        )
      )
  })                                          # ← ADD THIS CLOSING BRACE - it was missing
  
  output$choropleth_map <- renderLeaflet({    # ← now a separate, sibling block, not nested
    df <- filtered_data()
    
    zone_summary <- df %>%
      group_by(nearest_water_point) %>%
      summarise(cases = n(), .groups = "drop") %>%
      rename(zone = nearest_water_point) %>%
      left_join(zone_population, by = "zone") %>%
      mutate(attack_rate_per_1000 = round((cases / population) * 1000, 2))
    
    map_data <- zone_shapes %>%
      left_join(zone_summary, by = "zone")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$attack_rate_per_1000
    )
    
    leaflet(map_data) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(
        fillColor = ~pal(attack_rate_per_1000),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        popup = ~paste0(
          "<b>", zone, "</b><br>",
          "Cases: ", cases, "<br>",
          "Population: ", format(population, big.mark = ","), "<br>",
          "Attack Rate: ", attack_rate_per_1000, " per 1,000"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = map_data$attack_rate_per_1000,
        title = "Attack Rate<br>(per 1,000)",
        position = "bottomright"
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("cholera_surveillance_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$line_list_table <- renderDT({ filtered_data() })
}
shinyApp(ui, server)