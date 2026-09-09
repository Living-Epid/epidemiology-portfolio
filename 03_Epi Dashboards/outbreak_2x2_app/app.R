# Step 1: Install/load required packages for Project 4

packages <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "gt",          # polished 2x2 table rendering
  "epitools"     # OR/RR/confidence interval calculations - the core new tool
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)



# Step 2: Simulate a case-control study (wedding gastroenteritis outbreak)

set.seed(2024)

n_people <- 200  # total study participants (cases + controls)

# --- Simulate exposure to a specific food item (the suspected source) ---
# We'll test multiple exposures, but focus the 2x2 logic on one at a time
line_list <- data.frame(
  person_id = sprintf("P-%03d", 1:n_people),
  age = round(rnorm(n_people, mean = 38, sd = 14)),
  sex = sample(c("Male", "Female"), n_people, replace = TRUE, prob = c(0.48, 0.52))
)

# --- Simulate exposure to 3 food items served at the event ---
# Potato salad is the true culprit - built in with a real association
line_list$ate_potato_salad <- sample(c("Yes", "No"), n_people, replace = TRUE, prob = c(0.55, 0.45))
line_list$ate_chicken       <- sample(c("Yes", "No"), n_people, replace = TRUE, prob = c(0.70, 0.30))
line_list$ate_dessert       <- sample(c("Yes", "No"), n_people, replace = TRUE, prob = c(0.60, 0.40))

# --- Simulate illness outcome, driven mainly by potato salad exposure ---
# This creates a REAL statistical association for potato salad, weak/none for the others
illness_prob <- ifelse(line_list$ate_potato_salad == "Yes", 0.65, 0.12)
line_list$ill <- ifelse(runif(n_people) < illness_prob, "Case", "Control")

# --- Quick check ---
str(line_list)
table(line_list$ate_potato_salad, line_list$ill)



# Step 3: Define the UI

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#6A1B9A",
  danger = "#D32F2F",
  success = "#2E7D32",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- page_sidebar(
  title = "Outbreak Investigation: 2x2 Analysis",
  theme = my_theme,
  fillable = FALSE,   # <- turns off the stretch-to-fill behavior causing the scroll boxes
  
  sidebar = sidebar(
    title = "Analysis Settings",
    selectInput(
      inputId = "exposure_var",
      label = "Select Exposure to Test",
      choices = c(
        "Potato Salad" = "ate_potato_salad",
        "Chicken" = "ate_chicken",
        "Dessert" = "ate_dessert"
      ),
      selected = "ate_potato_salad"
    ),
    radioButtons(
      inputId = "study_type",
      label = "Study Design",
      choices = c("Case-Control (Odds Ratio)" = "cc", "Cohort (Relative Risk)" = "cohort"),
      selected = "cc"
    ),
    downloadButton("download_results", "Download Analysis Summary (CSV)", class = "btn-primary")
  ),
  
  layout_columns(
    value_box(title = "Total Participants", value = textOutput("total_n"), theme = "primary"),
    value_box(title = "Total Cases", value = textOutput("total_cases"), theme = "danger"),
    value_box(title = "Attack Rate Among Exposed (%)", value = textOutput("exposed_rate"), theme = "warning")
  ),
  
  navset_card_tab(
    nav_panel(
      "2x2 Table & Statistics",
      h4("2x2 Contingency Table"),
      gt_output("two_by_two_table"),
      hr(),
      h4("Epidemiological Measures"),
      gt_output("stats_table"),
      hr(),
      h4("Interpretation"),
      textOutput("interpretation_text")
    ),
    nav_panel(
      "Raw Data",
      DTOutput("line_list_table")
    )
  )
)


# Step 4: Statistical Server logic — 2x2 table construction, OR/RR calculation, interpretation

server <- function(input, output, session) {
  
  # --- Build the 2x2 table reactively based on selected exposure ---
  two_by_two <- reactive({
    exposure_col <- line_list[[input$exposure_var]]
    outcome_col <- line_list$ill
    
    # Order factors so "No"/"Control" are the reference (first) level - required for correct OR/RR direction
    exposure_f <- factor(exposure_col, levels = c("No", "Yes"))
    outcome_f  <- factor(outcome_col, levels = c("Control", "Case"))
    
    table(Exposure = exposure_f, Outcome = outcome_f)
  })
  
  # --- KPI value boxes ---
  output$total_n <- renderText({ nrow(line_list) })
  output$total_cases <- renderText({ sum(line_list$ill == "Case") })
  
  output$exposed_rate <- renderText({
    tab <- two_by_two()
    exposed_total <- sum(tab["Yes", ])
    exposed_cases <- tab["Yes", "Case"]
    if (exposed_total == 0) return("0")
    round(100 * exposed_cases / exposed_total, 1)
  })
  
  # --- Render the 2x2 table itself, gt-styled ---
  output$two_by_two_table <- render_gt({
    tab <- two_by_two()
    
    df <- data.frame(
      Exposure = c("Exposed (Yes)", "Unexposed (No)"),
      Case = c(tab["Yes", "Case"], tab["No", "Case"]),
      Control = c(tab["Yes", "Control"], tab["No", "Control"])
    )
    
    gt(df) %>%
      tab_header(title = "2x2 Contingency Table") %>%
      cols_label(Case = "Case", Control = "Control") %>%
      tab_style(
        style = cell_fill(color = "#F3E5F5"),
        locations = cells_body(columns = everything())
      )
  })
  
  # --- Calculate OR/RR with confidence intervals via epitools ---
  epi_stats <- reactive({
    tab <- two_by_two()
    
    # Zero-cell check - naive OR/RR formulas break (divide by zero) if any cell is 0
    if (any(tab == 0)) {
      return(list(error = TRUE))
    }
    
    if (input$study_type == "cc") {
      result <- oddsratio(tab, method = "wald")
      list(
        error = FALSE,
        measure = "Odds Ratio",
        estimate = round(result$measure[2, 1], 2),
        lower = round(result$measure[2, 2], 2),
        upper = round(result$measure[2, 3], 2),
        p_value = result$p.value[2, 2]              # display precision here
      )
    } else {
      result <- riskratio(tab, method = "wald")
      list(
        error = FALSE,
        measure = "Relative Risk",
        estimate = round(result$measure[2, 1], 2),
        lower = round(result$measure[2, 2], 2),
        upper = round(result$measure[2, 3], 2),
        p_value = result$p.value[2, 2]              # display precision here
      )
    }
  })
  
  # --- Render the stats table ---
  output$stats_table <- render_gt({
    stats <- epi_stats()
    
    if (stats$error) {
      df <- data.frame(Message = "One or more cells = 0. Cannot calculate a stable estimate. Try a different exposure variable.")
      return(gt(df))
    }
    
    df <- data.frame(
      Measure = stats$measure,
      Estimate = stats$estimate,
      `95% CI Lower` = stats$lower,
      `95% CI Upper` = stats$upper,
      `p-value` = ifelse(stats$p_value < 0.0001, "<0.0001", format(round(stats$p_value, 4), nsmall = 4)),
      check.names = FALSE
    )
    
    gt(df) %>%
      tab_header(title = paste(stats$measure, "with 95% Confidence Interval"))
  })
  
  # --- Auto-generated interpretation sentence (the real FETP report-writing skill) ---
  output$interpretation_text <- renderText({
    stats <- epi_stats()
    
    if (stats$error) {
      return("Interpretation unavailable due to a zero cell in the 2x2 table.")
    }
    
    exposure_labels <- c(
      "ate_potato_salad" = "Potato Salad",
      "ate_chicken" = "Chicken",
      "ate_dessert" = "Dessert"
    )
    exposure_label <- exposure_labels[[input$exposure_var]]
    
    sig_text <- if (stats$lower > 1 | stats$upper < 1) {
      "This association is statistically significant (the 95% CI does not include 1)."
    } else {
      "This association is not statistically significant (the 95% CI includes 1)."
    }
    
    paste0(
      "Individuals exposed to ", exposure_label, " had ", stats$estimate,
      " times the ", tolower(stats$measure), " of illness compared to unexposed individuals ",
      "(95% CI: ", stats$lower, "-", stats$upper, "). ", sig_text
    )
  })
  
  # --- Raw data table ---
  output$line_list_table <- renderDT({ line_list })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("outbreak_2x2_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      stats <- epi_stats()
      tab <- two_by_two()
      
      if (stats$error) {
        summary_df <- data.frame(Message = "Analysis unavailable due to a zero cell in the 2x2 table.")
      } else {
        exposure_labels <- c(
          "ate_potato_salad" = "Potato Salad",
          "ate_chicken" = "Chicken",
          "ate_dessert" = "Dessert"
        )
        
        summary_df <- data.frame(
          Exposure = exposure_labels[[input$exposure_var]],
          Study_Design = ifelse(input$study_type == "cc", "Case-Control", "Cohort"),
          Exposed_Cases = tab["Yes", "Case"],
          Exposed_Controls = tab["Yes", "Control"],
          Unexposed_Cases = tab["No", "Case"],
          Unexposed_Controls = tab["No", "Control"],
          Measure = stats$measure,
          Estimate = stats$estimate,
          CI_Lower = stats$lower,
          CI_Upper = stats$upper,
          P_Value = stats$p_value
        )
      }
      
      write.csv(summary_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
