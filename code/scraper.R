# Scraper for FTS Ebola data.
library(RCurl)
library(rjson)

# ScraperWiki deployed
onSw <- function(p = NULL, d = 'tool/', a = T) {
  if (a == T) return(paste0(d,p))
  else return(p)
}

# Loading helper funtions
source(onSw('code/summary.R')) # collect summary data
source(onSw('code/timeseries.R'))  # collect all financial data
source(onSw('code/indicators.R')) # for extracting indicators
source(onSw('code/parsecps.R')) # for storing in the required format
source(onSw('code/sw_status.R'))  # for changing status in SW
source(onSw('code/write_tables.R'))  # for writing db tables
source(onSw('tests/accuracy.R'))  # for executing value tests

# Function for fixing possible errors.
patchErrors <- function(df = NULL, v = NULL) {
  df <- df[df$docid != v,]
  return(df)
}

runScraper <- function(test = TRUE, patch = FALSE) {
  # Data collection
  fts_summary <- fetchSummary(1060)
  fts_timeseries <- fetchTimeSeries(16506)
  
  # Apply patch for data point from December 2015.
  # If errors are patched, tests will fail.
  if (patch) fts_timeseries <- patchErrors(df = fts_timeseries, v = c(224764))
  
  # Extracting indicators
  indicator_data <- exctractIndicators(fts_timeseries)
  
  # Storing the data (and running tests)
  if (test == T) accuracyTest(indicator_data, 1060)
  parseCPSFormat(indicator_data)
}

# Changing the status of SW.
tryCatch(runScraper(),
         error = function(e) {
           cat('Error detected ... sending notification.')
           system('mail -s "FTS Ebola failed." luiscape@gmail.com, takavarasha@un.org')
           changeSwStatus(type = "error", message = "Scraper failed.")
           { stop("!!") }
         }
)
# If success:
changeSwStatus(type = 'ok')