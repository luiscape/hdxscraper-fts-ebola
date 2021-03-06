# Function to parse data into 
# a CPS-friendly format
parseCPSFormat <- function(df) {
  
  ## Value:
  value = df
  
  ## Indicator schema:
  # indID
  # name
  # units
  indicator <- read.csv(paste0(onSw(), 'data/source/indicator.csv'))
  
  ## Dataset schema:
  # dsID
  # last_updated
  # last_scraped
  # name
  dataset <- data.frame(
    dsID = 'fts-ebola',
    last_updated = as.character(max(as.Date(value$period))),
    last_scraped = as.character(Sys.Date()),
    name = 'FTS Ebola Indicators'
  )
  
  ### Writing CSVs ###
  write.table(indicator, paste0(onSw(), 'data/indicator.csv'), row.names = F, col.names = F, sep = ",")
  write.table(dataset, paste0(onSw(), 'data/dataset.csv'), row.names = F, col.names = F, sep = ",")
  write.table(value, paste0(onSw(), 'data/value.csv'), row.names = F, col.names = F, sep = ",")
  cat('Done!\n')
  
  # Storing output.
  writeTables(indicator, "indicator", "scraperwiki")
  writeTables(dataset, "dataset", "scraperwiki")
  writeTables(value, "value", "scraperwiki")
}