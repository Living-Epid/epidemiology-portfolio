# getwd()
rsconnect::writeManifest()
# git add app.R manifest.json
# git commit -m "Add explicit library calls and manifest.json for capstone_sitrep deployment"
# git push



# Step 1: Install/load required packages for Project 6 (Capstone)

packages <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "lubridate",
  "scales",
  "plotly",
  "leaflet",
  "gt",
  "epitools"
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
library(plotly)
library(leaflet)
library(gt)
library(epitools)





# Step 2: Simulate a typhoid outbreak - festival point-source + community secondary spread

set.seed(2024)

n_cases <- 320

# --- Simulate case demographics + location ---
city_lat <- 6.5244
city_lon <- 3.3792

line_list <- data.frame(
  case_id = sprintf("TY-%04d", 1:n_cases),
  age = round(rgamma(n_cases, shape = 2.5, scale = 12)),
  sex = sample(c("Male", "Female"), n_cases, replace = TRUE, prob = c(0.49, 0.51)),
  district = sample(c("District 1", "District 2", "District 3", "District 4"),
                    n_cases, replace = TRUE, prob = c(0.30, 0.28, 0.24, 0.18))
)

# --- Coordinates: cluster around the district centers with some spread ---
district_coords <- data.frame(
  district = c("District 1", "District 2", "District 3", "District 4"),
  lat = c(city_lat + 0.02, city_lat - 0.015, city_lat + 0.01, city_lat - 0.025),
  lon = c(city_lon - 0.02, city_lon + 0.015, city_lon + 0.03, city_lon - 0.01)
)

line_list <- line_list %>%
  left_join(district_coords, by = "district") %>%
  mutate(
    lat = lat + rnorm(n_cases, 0, 0.008),
    lon = lon + rnorm(n_cases, 0, 0.008)
  )

# --- Exposure: attended the festival (point source) ---
line_list$attended_festival <- sample(c("Yes", "No"), n_cases, replace = TRUE, prob = c(0.45, 0.55))

# --- Water source (secondary risk factor, for the 2x2 analytic tab) ---
line_list$water_source <- sample(
  c("Piped municipal", "Unprotected well", "Borehole", "Vendor/Tanker"),
  n_cases, replace = TRUE, prob = c(0.40, 0.30, 0.20, 0.10)
)

# --- Onset timeline: festival attendees cluster early (point-source peak), ---
# --- non-attendees show a longer, flatter tail (community/secondary spread) ---
outbreak_start <- as.Date("2024-04-01")

festival_day_weights <- dgamma(1:70, shape = 3, rate = 0.35)   # sharp early peak
community_day_weights <- dgamma(1:70, shape = 2, rate = 0.12)  # slower, longer tail

line_list$onset_day <- ifelse(
  line_list$attended_festival == "Yes",
  sample(1:70, n_cases, replace = TRUE, prob = festival_day_weights),
  sample(1:70, n_cases, replace = TRUE, prob = community_day_weights)
)

line_list$date_onset <- outbreak_start + line_list$onset_day
line_list$date_reported <- line_list$date_onset + sample(0:5, n_cases, replace = TRUE)

# --- Illness confirmed vs suspected (real surveillance systems track both) ---
line_list$case_classification <- sample(c("Confirmed", "Suspected"), n_cases, replace = TRUE, prob = c(0.35, 0.65))

# --- Outcome, with higher severity for unprotected well exposure ---
severity_risk <- ifelse(line_list$water_source == "Unprotected well", 0.15, 0.04)
line_list$outcome <- ifelse(runif(n_cases) < severity_risk, "Died", "Recovered")

# --- Quick check ---
str(line_list)
head(line_list, 5)
table(line_list$attended_festival, cut(line_list$onset_day, breaks = c(0, 15, 30, 45, 60, 70)))






# Step 3: Define the UI - multi-tab capstone architecture

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#B71C1C",      # deep red - situation report / alert tone, distinct from prior projects
  danger = "#D32F2F",
  warning = "#F57C00",
  success = "#2E7D32",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Typhoid Outbreak Situation Report",
  theme = my_theme,
  fillable = FALSE,
  
  sidebar = sidebar(
    title = "Global Filters",
    selectInput(
      inputId = "district_filter",
      label = "District",
      choices = c("All Districts", unique(line_list$district)),
      selected = "All Districts"
    ),
    selectInput(
      inputId = "classification_filter",
      label = "Case Classification",
      choices = c("All", "Confirmed", "Suspected"),
      selected = "All"
    ),
    hr(),
    downloadButton("download_data", "Download Filtered Data (CSV)", class = "btn-primary")
  ),
  
  layout_columns(
    value_box(title = "Total Cases", value = textOutput("total_cases"), theme = "primary"),
    value_box(title = "Confirmed Cases", value = textOutput("confirmed_cases"), theme = "success"),
    value_box(title = "Deaths", value = textOutput("total_deaths"), theme = "danger"),
    value_box(title = "CFR (%)", value = textOutput("cfr"), theme = "warning")
  ),
  
  navset_tab(
    nav_panel(
      "Descriptive Epi",
      h4("Epidemic Curve: Festival Attendees vs. Community Cases"),
      plotlyOutput("epi_curve", height = "400px"),
      hr(),
      h4("Case Line List"),
      DTOutput("line_list_table")
    ),
    nav_panel(
      "Spatial Distribution",
      h4("Case Map by District"),
      leafletOutput("case_map", height = "500px")
    ),
    nav_panel(
      "Analytic Epi",
      h4("Exposure Analysis"),
      selectInput(
        inputId = "exposure_var",
        label = "Select Exposure",
        choices = c(
          "Attended Festival" = "attended_festival",
          "Unprotected Well Water" = "unprotected_well"
        ),
        selected = "attended_festival"
      ),
      gt_output("two_by_two_table"),
      hr(),
      gt_output("stats_table"),
      hr(),
      textOutput("interpretation_text")
    ),
    nav_panel(
      "Situation Report Summary",
      uiOutput("sitrep_summary")
    )
  )
)






# Step 4: Server logic part 1 - filtering, KPIs, epi curve, line list table

server <- function(input, output, session) {
  
  # --- Create binary exposure column for unprotected well (needed for Analytic Epi tab) ---
  line_list$unprotected_well <- ifelse(line_list$water_source == "Unprotected well", "Yes", "No")
  
  # --- Core reactive filter (applies globally across all tabs) ---
  filtered_data <- reactive({
    data <- line_list
    
    if (input$district_filter != "All Districts") {
      data <- data[data$district == input$district_filter, ]
    }
    
    if (input$classification_filter != "All") {
      data <- data[data$case_classification == input$classification_filter, ]
    }
    
    data
  })
  
  # --- KPI value boxes ---
  output$total_cases <- renderText({ nrow(filtered_data()) })
  output$confirmed_cases <- renderText({ sum(filtered_data()$case_classification == "Confirmed") })
  output$total_deaths <- renderText({ sum(filtered_data()$outcome == "Died") })
  
  output$cfr <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    round(100 * sum(df$outcome == "Died") / nrow(df), 1)
  })
  
  # --- Epi curve: festival attendees vs. community cases, overlaid ---
  output$epi_curve <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = date_onset, fill = attended_festival)) +
      geom_histogram(binwidth = 2, position = "stack", color = "white") +
      scale_fill_manual(values = c("Yes" = "#B71C1C", "No" = "#546E7A"),
                        labels = c("Yes" = "Attended Festival", "No" = "Community/Other")) +
      labs(x = "Date of Onset", y = "Number of Cases", fill = "Exposure") +
      theme_minimal(base_size = 12)
    
    ggplotly(p)
  })
  
  # --- Line list table ---
  output$line_list_table <- renderDT({ filtered_data() })
  
  # --- Download handler ---
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("typhoid_sitrep_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # --- Spatial map ---
  output$case_map <- renderLeaflet({
    df <- filtered_data()
    
    leaflet(df) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5,
        color = ~ifelse(outcome == "Died", "#D32F2F", "#B71C1C"),
        fillOpacity = 0.6,
        stroke = FALSE,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b>", case_id, "</b><br>",
          "District: ", district, "<br>",
          "Onset: ", date_onset, "<br>",
          "Classification: ", case_classification, "<br>",
          "Outcome: ", outcome
        )
      )
  })
  
  # --- 2x2 table (reused pattern from Project 4) ---
  two_by_two <- reactive({
    exposure_col <- filtered_data()[[input$exposure_var]]
    outcome_col <- ifelse(filtered_data()$case_classification == "Confirmed", "Case", "Control")
    
    exposure_f <- factor(exposure_col, levels = c("No", "Yes"))
    outcome_f  <- factor(outcome_col, levels = c("Control", "Case"))
    
    table(Exposure = exposure_f, Outcome = outcome_f)
  })
  
  output$two_by_two_table <- render_gt({
    tab <- two_by_two()
    
    df <- data.frame(
      Exposure = c("Exposed (Yes)", "Unexposed (No)"),
      Case = c(tab["Yes", "Case"], tab["No", "Case"]),
      Control = c(tab["Yes", "Control"], tab["No", "Control"])
    )
    
    gt(df) %>%
      tab_header(title = "2x2 Contingency Table") %>%
      tab_style(style = cell_fill(color = "#FFEBEE"), locations = cells_body(columns = everything()))
  })
  
  epi_stats <- reactive({
    tab <- two_by_two()
    
    if (any(tab == 0)) return(list(error = TRUE))
    
    result <- oddsratio(tab, method = "wald")
    list(
      error = FALSE,
      estimate = round(result$measure[2, 1], 2),
      lower = round(result$measure[2, 2], 2),
      upper = round(result$measure[2, 3], 2),
      p_value = result$p.value[2, 2]
    )
  })
  
  output$stats_table <- render_gt({
    stats <- epi_stats()
    
    if (stats$error) {
      return(gt(data.frame(Message = "One or more cells = 0. Try a different exposure or filter.")))
    }
    
    df <- data.frame(
      Measure = "Odds Ratio",
      Estimate = stats$estimate,
      `95% CI Lower` = stats$lower,
      `95% CI Upper` = stats$upper,
      `p-value` = ifelse(stats$p_value < 0.0001, "<0.0001", format(round(stats$p_value, 4), nsmall = 4)),
      check.names = FALSE
    )
    
    gt(df) %>% tab_header(title = "Odds Ratio with 95% Confidence Interval")
  })
  
  output$interpretation_text <- renderText({
    stats <- epi_stats()
    
    if (stats$error) return("Interpretation unavailable due to a zero cell in the 2x2 table.")
    
    exposure_labels <- c(
      "attended_festival" = "Attending the Festival",
      "unprotected_well" = "Unprotected Well Water Exposure"
    )
    exposure_label <- exposure_labels[[input$exposure_var]]
    
    sig_text <- if (stats$lower > 1 | stats$upper < 1) {
      "This association is statistically significant."
    } else {
      "This association is not statistically significant."
    }
    
    paste0(
      exposure_label, " was associated with ", stats$estimate,
      " times the odds of being a confirmed case (95% CI: ", stats$lower, "-", stats$upper, "). ",
      sig_text
    )
  })
  
  output$sitrep_summary <- renderUI({
    df <- filtered_data()
    
    total <- nrow(df)
    confirmed <- sum(df$case_classification == "Confirmed")
    deaths <- sum(df$outcome == "Died")
    cfr_val <- if (total > 0) round(100 * deaths / total, 1) else 0
    festival_pct <- if (total > 0) round(100 * sum(df$attended_festival == "Yes") / total, 1) else 0
    
    date_range <- if (total > 0) {
      paste(format(min(df$date_onset), "%d %b %Y"), "to", format(max(df$date_onset), "%d %b %Y"))
    } else {
      "N/A"
    }
    
    top_district <- if (total > 0) {
      df %>% count(district, sort = TRUE) %>% slice(1) %>% pull(district)
    } else {
      "N/A"
    }
    
    tagList(
      div(
        style = "background-color: #FFF3E0; padding: 20px; border-left: 5px solid #B71C1C; margin-bottom: 20px;",
        h3(paste("Situation Report -", format(Sys.Date(), "%d %B %Y"))),
        p(strong("Reporting Period: "), date_range)
      ),
      
      h4("1. Summary"),
      p(paste0(
        "As of ", format(Sys.Date(), "%d %B %Y"), ", a total of ", total,
        " typhoid cases have been reported (", confirmed, " confirmed, ",
        total - confirmed, " suspected). ", deaths, " deaths have occurred, ",
        "for a case fatality rate of ", cfr_val, "%. ",
        festival_pct, "% of cases reported attending the community festival, ",
        "identified as a likely point-source exposure event."
      )),
      
      h4("2. Geographic Distribution"),
      p(paste0("The most affected area is ", top_district, ". Case mapping shows clustering ",
               "consistent with both point-source exposure and secondary community transmission.")),
      
      h4("3. Epidemic Curve Interpretation"),
      p("The epidemic curve shows an early, sharp peak among festival attendees consistent with ",
        "a common point-source exposure, followed by a longer, lower-level tail among non-attendees ",
        "consistent with secondary person-to-person transmission within the community."),
      h4("4. Recommendations"),
      tags$ul(
        tags$li("Continue active case finding in the most affected district(s)."),
        tags$li("Investigate water sources in affected areas, prioritizing unprotected wells."),
        tags$li("Consider public health messaging regarding safe water and food handling practices."),
        tags$li("Continue monitoring for secondary transmission beyond the point-source exposure window.")
      )
    )
  })
  
}

shinyApp(ui, server)
