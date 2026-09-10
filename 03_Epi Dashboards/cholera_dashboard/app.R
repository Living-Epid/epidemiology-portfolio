# Step 1: Environment Setup & Package Installation


packages <- c(
  "shiny",       # core Shiny framework
  "bslib",       # modern Bootstrap 5 theming for Shiny
  "DT",          # interactive data tables (line list display)
  "dplyr",       # data wrangling
  "lubridate",   # date handling (onset dates, MMWR weeks)
  "reactable",   # polished alternative table option
  "scales",       # number/percent formatting for summary cards
  "plotly"       # interactive charts (epi curve, later maps)
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
library(reactable)
library(scales)
library(plotly)



# Step 2: Simulate a cholera outbreak line list (WHO/FETP-style structure)

set.seed(2024)  # reproducibility - keep this fixed while we're learning

n_cases <- 350

# --- Simulate case demographics ---
line_list <- data.frame(
  case_id     = sprintf("CH-%04d", 1:n_cases),
  age         = round(rgamma(n_cases, shape = 2, scale = 12)),
  sex         = sample(c("Male", "Female"), n_cases, replace = TRUE, prob = c(0.48, 0.52)),
  ward        = sample(
    c("Ward A - Riverside", "Ward B - Market District", "Ward C - Highlands",
      "Ward D - Old Town", "Ward E - New Settlement"),
    n_cases, replace = TRUE, prob = c(0.30, 0.25, 0.10, 0.20, 0.15)
  ),
  water_source = sample(
    c("Unprotected well", "River/Stream", "Piped municipal", "Borehole", "Vendor/Tanker"),
    n_cases, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.15, 0.10)
  )
)

# --- Simulate an epidemic curve (outbreak rises, peaks, declines) ---
outbreak_start <- as.Date("2024-03-01")
day_weights <- dgamma(1:60, shape = 3, rate = 0.25)  # rise-peak-decline shape
onset_days  <- sample(1:60, n_cases, replace = TRUE, prob = day_weights)

line_list$date_onset    <- outbreak_start + onset_days
line_list$date_reported <- line_list$date_onset + sample(0:4, n_cases, replace = TRUE)

# --- Simulate clinical outcome ---
# Younger children and elderly have higher fatality risk (realistic pattern)
risk_score <- ifelse(line_list$age < 5 | line_list$age > 60, 0.06, 0.015)
line_list$outcome <- ifelse(runif(n_cases) < risk_score, "Died", "Recovered")

line_list$hospitalized <- sample(c("Yes", "No"), n_cases, replace = TRUE, prob = c(0.55, 0.45))

# --- Quick check ---
str(line_list)
head(line_list, 10)



# Ward population denominators (simulated census/catchment data)

ward_population <- data.frame(
  ward = c("Ward A - Riverside", "Ward B - Market District", "Ward C - Highlands",
           "Ward D - Old Town", "Ward E - New Settlement"),
  population = c(8200, 15400, 4100, 9800, 6300)
)



# Step 3: Define the UI (User Interface) skeleton with bslib Theming

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#005EB8",      # WHO blue
  danger = "#D32F2F",       # for death/CFR indicators
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Cholera Outbreak Investigation Dashboard",
  theme = my_theme,
  
  sidebar = sidebar(
    title = "Filters",
    selectInput(
      inputId = "ward_filter",
      label = "Ward",
      choices = c("All Wards", unique(line_list$ward)),
      selected = "All Wards"
    ),
    selectInput(
      inputId = "outcome_filter",
      label = "Outcome",
      choices = c("All", unique(line_list$outcome)),
      selected = "All"
    ),
    downloadButton("download_data", "Download Filtered Data (CSV)", class = "btn-primary")
  ),
  
  layout_columns(
    value_box(title = "Total Cases", value = textOutput("total_cases"), theme = "primary"),
    value_box(title = "Deaths", value = textOutput("total_deaths"), theme = "danger"),
    value_box(title = "CFR (%)", value = textOutput("cfr"), theme = "warning")
  ),
  
  navset_card_tab(
    nav_panel(
      "Case Line List",
      DTOutput("line_list_table")
    ),
    nav_panel(
      "Epi Curve",
      plotlyOutput("epi_curve", height = "400px")
    ),
    nav_panel(
      "Attack Rate by Ward",
      reactableOutput("attack_rate_table")
    )
  )
)




# Step 4: Reactive filtering — the value boxes and table now respond to input

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- line_list
    
    if (input$ward_filter != "All Wards") {
      data <- data[data$ward == input$ward_filter, ]
    }
    
    if (input$outcome_filter != "All") {
      data <- data[data$outcome == input$outcome_filter, ]
    }
    
    data
  })
  
  output$total_cases <- renderText({ nrow(filtered_data()) })
  
  output$total_deaths <- renderText({ sum(filtered_data()$outcome == "Died") })
  
  output$cfr <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    round(100 * sum(df$outcome == "Died") / nrow(df), 1)
  })
  
  output$epi_curve <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = date_onset, fill = outcome)) +
      geom_histogram(binwidth = 1, color = "white") +
      scale_fill_manual(values = c("Recovered" = "#005EB8", "Died" = "#D32F2F")) +
      labs(x = "Date of Onset", y = "Number of Cases", fill = "Outcome") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  output$attack_rate_table <- renderReactable({
    df <- filtered_data()
    
    ward_summary <- df %>%
      group_by(ward) %>%
      summarise(cases = n(), .groups = "drop") %>%
      left_join(ward_population, by = "ward") %>%
      mutate(
        attack_rate_per_1000 = round((cases / population) * 1000, 2)
      ) %>%
      arrange(desc(attack_rate_per_1000))
    
    reactable(
      ward_summary,
      columns = list(
        ward = colDef(name = "Ward"),
        cases = colDef(name = "Cases"),
        population = colDef(name = "Population", format = colFormat(separators = TRUE)),
        attack_rate_per_1000 = colDef(name = "Attack Rate (per 1,000)")
      ),
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("cholera_line_list_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$line_list_table <- renderDT({ filtered_data() })
}


shinyApp(ui, server)
