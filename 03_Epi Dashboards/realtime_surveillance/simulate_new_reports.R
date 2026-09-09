# simulate_new_reports.R
# Run this WHILE app.R (the dashboard) is running, to simulate live incoming case reports

library(DBI)
library(RSQLite)

db_path <- "surveillance.sqlite"

add_new_case <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  new_id <- sprintf("AWD-%05d", as.integer(Sys.time()))  # simple unique-ish ID using timestamp
  
  new_case <- data.frame(
    case_id = new_id,
    age = round(rgamma(1, shape = 2, scale = 15)),
    sex = sample(c("Male", "Female"), 1),
    district = sample(c("District 1", "District 2", "District 3"), 1),
    date_reported = as.character(Sys.Date()),
    severity = sample(c("Mild", "Moderate", "Severe"), 1, prob = c(0.6, 0.3, 0.1))
  )
  
  dbWriteTable(con, "cases", new_case, append = TRUE)
  dbDisconnect(con)
  
  cat("Added new case:", new_id, "at", format(Sys.time(), "%H:%M:%S"), "\n")
}

# Add one new case right now
add_new_case()
