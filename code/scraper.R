# Scraper for FTS Ebola data.
library(RCurl)
library(rjson)

# ScraperWiki deployed
onSw <- function(a = T, d = 'tool/') {
  if (a == T) return(d)
  else return('')
}

# Loading helper funtions
source(paste0(onSw(), 'code/summary.R'))  # collect summary data
source(paste0(onSw(), 'code/timeseries.R'))  # collect all financial data
source(paste0(onSw(), 'code/indicators.R'))  # for extracting indicators
source(paste0(onSw(), 'code/parsecps.R'))  # for storing in the required format
source(paste0(onSw(), 'code/sw_status.R'))  # for changing status in SW
source(paste0(onSw(), 'code/write_tables.R'))  # for writing db tables
source(paste0(onSw(), 'tests/accuracy.R'))  # for writing db tables

runScraper <- function(test = T) {
  # Data collection
  fts_summary <- fetchSummary(1060)
  fts_timeseries <- fetchTimeSeries(16506)
  
  # Extracting indicators
  indicator_data <- exctractIndicators(fts_timeseries)
  
  # Storing the data (and running tests)
  if (test == T) accuracyTest(indicator_data, 1060)
  parseCPSFormat(indicator_data)
}

# Changing the status of SW.
tryCatch(runScraper(F),
         error = function(e) {
           cat('Error detected ... sending notification.')
           system('mail -s "FTS Ebola failed." luiscape@gmail.com, takavarasha@un.org')
           changeSwStatus(type = "error", message = "Scraper failed.")
           { stop("!!") }
         }
)
# If success:
changeSwStatus(type = 'ok')