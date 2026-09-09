# Step 1: Install/load required packages for Project 2

packages <- c(
  "shiny",        # core Shiny framework
  "bslib",        # Bootstrap 5 theming
  "DT",           # interactive tables
  "dplyr",        # data wrangling
  "lubridate",    # date handling
  "reactable",    # polished tables (attack rate style)
  "scales",       # number/percent formatting
  "plotly",       # interactive charts
  "tidyr",        # reshaping data for multi-series plots
  "zoo",          # rolling/moving average calculation
  "MMWRweek"      # official CDC epi week conversion
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)




# Step 2: Simulate a measles outbreak line list (WHO/CDC-style structure)

set.seed(2024)

n_cases <- 280

# --- Simulate case demographics ---
line_list <- data.frame(
  case_id = sprintf("MEA-%04d", 1:n_cases),
  age = round(rgamma(n_cases, shape = 2, scale = 6)),  # measles skews younger
  sex = sample(c("Male", "Female"), n_cases, replace = TRUE, prob = c(0.50, 0.50)),
  region = sample(
    c("Region North", "Region Central", "Region South", "Region East", "Region West"),
    n_cases, replace = TRUE, prob = c(0.28, 0.22, 0.18, 0.17, 0.15)
  )
)

# --- Vaccination status (core measles-specific variable) ---
# Younger, unvaccinated children drive most outbreaks - build that pattern in
line_list$vaccination_status <- ifelse(
  line_list$age < 5,
  sample(c("Unvaccinated", "1 Dose", "2 Doses"), n_cases, replace = TRUE, prob = c(0.55, 0.30, 0.15)),
  sample(c("Unvaccinated", "1 Dose", "2 Doses"), n_cases, replace = TRUE, prob = c(0.20, 0.30, 0.50))
)

# --- Simulate outbreak timeline across ~14 weeks ---
outbreak_start <- as.Date("2024-01-08")  # start on a Monday for clean MMWR alignment
day_weights <- dgamma(1:98, shape = 4, rate = 0.3)  # slower rise, longer tail than cholera
onset_days <- sample(1:98, n_cases, replace = TRUE, prob = day_weights)

line_list$date_onset <- outbreak_start + onset_days
line_list$date_reported <- line_list$date_onset + sample(0:7, n_cases, replace = TRUE)

# --- Clinical outcome & complications ---
# Unvaccinated cases have higher complication risk (realistic pattern)
complication_risk <- ifelse(line_list$vaccination_status == "Unvaccinated", 0.18, 0.05)
line_list$complications <- ifelse(runif(n_cases) < complication_risk, "Yes", "No")

line_list$hospitalized <- ifelse(line_list$complications == "Yes",
                                 sample(c("Yes", "No"), n_cases, replace = TRUE, prob = c(0.75, 0.25)),
                                 sample(c("Yes", "No"), n_cases, replace = TRUE, prob = c(0.15, 0.85)))

# --- Quick check ---
str(line_list)
head(line_list, 10)

# Simulated regional population by vaccination status (denominator data)

pop_by_vax <- data.frame(
  vaccination_status = c("Unvaccinated", "1 Dose", "2 Doses"),
  population = c(4200, 9800, 38000)  # unvaccinated pool is deliberately small - realistic for high-coverage areas
)




# Step 3: Convert onset dates to MMWR epi week + year

mmwr_data <- MMWRweek(line_list$date_onset)

line_list$mmwr_year <- mmwr_data$MMWRyear
line_list$mmwr_week <- mmwr_data$MMWRweek

# Create a combined, sortable label for charting (e.g. "2024-W03")
line_list$epi_week_label <- sprintf("%d-W%02d", line_list$mmwr_year, line_list$mmwr_week)

# --- Quick check ---
head(line_list[, c("case_id", "date_onset", "mmwr_year", "mmwr_week", "epi_week_label")], 10)




# Step 4: Define the UI with bslib theming, filters, and tab layout

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#00843D",       # measles/immunization green (WHO EPI branding tone)
  danger = "#D32F2F",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Measles Outbreak Surveillance Dashboard",
  theme = my_theme,
  
  sidebar = sidebar(
    title = "Filters",
    selectInput(
      inputId = "region_filter",
      label = "Region",
      choices = c("All Regions", unique(line_list$region)),
      selected = "All Regions"
    ),
    selectInput(
      inputId = "vax_filter",
      label = "Vaccination Status",
      choices = c("All", unique(line_list$vaccination_status)),
      selected = "All"
    ),
    dateRangeInput(
      inputId = "date_range",
      label = "Onset Date Range",
      start = min(line_list$date_onset),
      end = max(line_list$date_onset)
    ),
    downloadButton("download_data", "Download Filtered Data (CSV)", class = "btn-primary")
  ),
  
  layout_columns(
    value_box(title = "Total Cases", value = textOutput("total_cases"), theme = "primary"),
    value_box(title = "Unvaccinated (%)", value = textOutput("pct_unvax"), theme = "danger"),
    value_box(title = "Complication Rate (%)", value = textOutput("complication_rate"), theme = "warning")
  ),
  
  navset_card_tab(
    nav_panel(
      "Weekly Epi Curve",
      plotlyOutput("epi_curve_weekly", height = "420px")
    ),
    nav_panel(
      "By Vaccination Status",
      plotlyOutput("epi_curve_by_vax", height = "420px")
    ),
    nav_panel(
      "Case Line List",
      DTOutput("line_list_table")
    ),
    nav_panel(
      "Vaccine Effectiveness",
      reactableOutput("vax_effectiveness_table")
    )
  )
)





# Step 5: Server logic — filtering, KPIs, and weekly aggregation

server <- function(input, output, session) {
  
  # --- Core reactive filter ---
  filtered_data <- reactive({
    data <- line_list
    
    if (input$region_filter != "All Regions") {
      data <- data[data$region == input$region_filter, ]
    }
    
    if (input$vax_filter != "All") {
      data <- data[data$vaccination_status == input$vax_filter, ]
    }
    
    data <- data[data$date_onset >= input$date_range[1] &
                   data$date_onset <= input$date_range[2], ]
    
    data
  })
  
  # --- KPI value boxes ---
  output$total_cases <- renderText({ nrow(filtered_data()) })
  
  output$pct_unvax <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    round(100 * sum(df$vaccination_status == "Unvaccinated") / nrow(df), 1)
  })
  
  output$complication_rate <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    round(100 * sum(df$complications == "Yes") / nrow(df), 1)
  })
  
  # --- Weekly aggregation for epi curve (this is the new core skill) ---
  weekly_summary <- reactive({
    df <- filtered_data()
    
    df %>%
      group_by(mmwr_year, mmwr_week, epi_week_label) %>%
      summarise(cases = n(), .groups = "drop") %>%
      arrange(mmwr_year, mmwr_week) %>%
      mutate(rolling_avg = zoo::rollmean(cases, k = 3, fill = NA, align = "right"))
  })
  
  output$epi_curve_weekly <- renderPlotly({
    wk <- weekly_summary()
    
    p <- ggplot(wk, aes(x = reorder(epi_week_label, mmwr_year * 100 + mmwr_week))) +
      geom_col(aes(y = cases), fill = "#00843D") +
      geom_line(aes(y = rolling_avg, group = 1), color = "#D32F2F", linewidth = 1) +
      labs(x = "MMWR Epi Week", y = "Number of Cases",
           title = "Weekly Case Counts (bars) with 3-Week Rolling Average (line)") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # --- Multi-series chart by vaccination status ---
  output$epi_curve_by_vax <- renderPlotly({
    df <- filtered_data()
    
    vax_weekly <- df %>%
      group_by(mmwr_year, mmwr_week, epi_week_label, vaccination_status) %>%
      summarise(cases = n(), .groups = "drop") %>%
      arrange(mmwr_year, mmwr_week)
    
    p <- ggplot(vax_weekly, aes(
      x = reorder(epi_week_label, mmwr_year * 100 + mmwr_week),
      y = cases,
      color = vaccination_status,
      group = vaccination_status
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      scale_color_manual(values = c(
        "Unvaccinated" = "#D32F2F",
        "1 Dose" = "#F5A623",
        "2 Doses" = "#00843D"
      )) +
      labs(x = "MMWR Epi Week", y = "Cases", color = "Vaccination Status") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # --- Line list table ---
  output$line_list_table <- renderDT({ filtered_data() })
  
  # --- Download handler ---
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("measles_line_list_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$vax_effectiveness_table <- renderReactable({
    df <- filtered_data()
    
    vax_summary <- df %>%
      group_by(vaccination_status) %>%
      summarise(cases = n(), .groups = "drop") %>%
      left_join(pop_by_vax, by = "vaccination_status") %>%
      mutate(
        attack_rate_per_1000 = round((cases / population) * 1000, 2)
      )
    
    # Relative risk vs unvaccinated (the reference group)
    unvax_rate <- vax_summary$attack_rate_per_1000[vax_summary$vaccination_status == "Unvaccinated"]
    
    vax_summary <- vax_summary %>%
      mutate(
        relative_risk = round(attack_rate_per_1000 / unvax_rate, 3),
        vaccine_effectiveness_pct = round((1 - relative_risk) * 100, 1)
      ) %>%
      arrange(desc(attack_rate_per_1000))
    
    reactable(
      vax_summary,
      columns = list(
        vaccination_status = colDef(name = "Vaccination Status"),
        cases = colDef(name = "Cases"),
        population = colDef(name = "Population", format = colFormat(separators = TRUE)),
        attack_rate_per_1000 = colDef(name = "Attack Rate (per 1,000)"),
        relative_risk = colDef(name = "Relative Risk (vs. Unvaccinated)"),
        vaccine_effectiveness_pct = colDef(name = "Vaccine Effectiveness (%)")
      ),
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE
    )
  })
  
}

shinyApp(ui, server)