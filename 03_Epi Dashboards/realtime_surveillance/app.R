# Step 1: Install/load required packages for Project 5

packages <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "lubridate",
  "plotly",
  "DBI",         # database interface - the core new tool
  "RSQLite"      # SQLite database engine
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)




# Step 2: Create/connect to SQLite database and seed initial case data

db_path <- "surveillance.sqlite"

con <- dbConnect(RSQLite::SQLite(), db_path)

# --- Create the cases table if it doesn't already exist ---
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS cases (
    case_id TEXT PRIMARY KEY,
    age INTEGER,
    sex TEXT,
    district TEXT,
    date_reported TEXT,
    severity TEXT
  )
")

# --- Seed with an initial batch of cases (only if table is empty) ---
existing_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM cases")$n

if (existing_count == 0) {
  set.seed(2024)
  n_seed <- 40
  
  seed_data <- data.frame(
    case_id = sprintf("AWD-%05d", 1:n_seed),
    age = round(rgamma(n_seed, shape = 2, scale = 15)),
    sex = sample(c("Male", "Female"), n_seed, replace = TRUE),
    district = sample(c("District 1", "District 2", "District 3"), n_seed, replace = TRUE),
    date_reported = as.character(Sys.Date() - sample(0:6, n_seed, replace = TRUE)),
    severity = sample(c("Mild", "Moderate", "Severe"), n_seed, replace = TRUE, prob = c(0.6, 0.3, 0.1))
  )
  
  dbWriteTable(con, "cases", seed_data, append = TRUE)
}

# --- Quick check ---
dbGetQuery(con, "SELECT * FROM cases LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) as total_cases FROM cases")

dbDisconnect(con)




# Step 3: Define the UI

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#00695C",     # teal - distinct from previous projects
  danger = "#D32F2F",
  warning = "#F57C00",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Real-Time AWD Surveillance Dashboard",
  theme = my_theme,
  fillable = FALSE,
  
  sidebar = sidebar(
    title = "Settings",
    selectInput(
      inputId = "district_filter",
      label = "District",
      choices = c("All Districts", "District 1", "District 2", "District 3"),
      selected = "All Districts"
    ),
    numericInput(
      inputId = "alert_threshold",
      label = "Alert Threshold (cases/day)",
      value = 10,
      min = 1
    ),
    p(strong("Last refreshed:")),
    textOutput("last_refresh_time"),
    downloadButton("download_data", "Download Current Cases (CSV)", class = "btn-primary")   # ← moved here
  ),                                                                                          # ← closes sidebar(...)
  
  layout_columns(
    value_box(title = "Total Cases (7 days)", value = textOutput("total_cases"), theme = "primary"),
    value_box(title = "Severe Cases", value = textOutput("severe_cases"), theme = "danger"),
    value_box(title = "Alert Status", value = textOutput("alert_status"), theme = "warning")
  ),
  
  navset_card_tab(
    nav_panel(
      "Daily Case Trend",
      plotlyOutput("daily_trend", height = "400px")
    ),
    nav_panel(
      "Recent Cases",
      DTOutput("cases_table")
    )
  )
)



# Step 4: Server logic with reactivePoll for real-time database refresh

server <- function(input, output, session) {
  
  db_path <- "surveillance.sqlite"
  
  # --- reactivePoll checks for changes every 5 seconds and re-reads the DB if the file changed ---
  live_data <- reactivePoll(
    intervalMillis = 5000,   # check every 5 seconds
    session = session,
    
    # checkFunc: a cheap check to see if anything changed (file modification time)
    checkFunc = function() {
      if (file.exists(db_path)) file.info(db_path)$mtime else ""
    },
    
    # valueFunc: only runs if checkFunc's return value changed since last check
    valueFunc = function() {
      con <- dbConnect(RSQLite::SQLite(), db_path)
      data <- dbGetQuery(con, "SELECT * FROM cases")
      dbDisconnect(con)
      
      data$date_reported <- as.Date(data$date_reported)
      data
    }
  )
  
  # --- Apply district filter on top of the live data ---
  filtered_data <- reactive({
    data <- live_data()
    
    if (input$district_filter != "All Districts") {
      data <- data[data$district == input$district_filter, ]
    }
    
    data
  })
  
  # --- Last refresh timestamp, for user reassurance that it's actually live ---
  output$last_refresh_time <- renderText({
    live_data()  # triggers re-run whenever new data is detected
    format(Sys.time(), "%H:%M:%S")
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("awd_surveillance_", Sys.Date(), "_", format(Sys.time(), "%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # --- KPI value boxes ---
  output$total_cases <- renderText({ nrow(filtered_data()) })
  
  output$severe_cases <- renderText({
    sum(filtered_data()$severity == "Severe")
  })
  
  output$alert_status <- renderText({
    df <- filtered_data()
    today_count <- sum(df$date_reported == Sys.Date())
    
    if (today_count >= input$alert_threshold) {
      "ALERT"
    } else {
      "Normal"
    }
  })
  
  # --- Daily trend chart ---
  output$daily_trend <- renderPlotly({
    df <- filtered_data()
    
    daily_summary <- df %>%
      group_by(date_reported) %>%
      summarise(cases = n(), .groups = "drop") %>%
      arrange(date_reported)
    
    p <- ggplot(daily_summary, aes(x = date_reported, y = cases)) +
      geom_col(fill = "#00695C") +
      geom_hline(yintercept = input$alert_threshold, color = "#D32F2F", linetype = "dashed", linewidth = 1) +
      labs(x = "Date", y = "Cases Reported", title = "Daily Case Counts (red line = alert threshold)") +
      theme_minimal(base_size = 12)
    
    ggplotly(p)
  })
  
  # --- Recent cases table ---
  output$cases_table <- renderDT({
    filtered_data() %>% arrange(desc(date_reported))
  })
}

shinyApp(ui, server)
