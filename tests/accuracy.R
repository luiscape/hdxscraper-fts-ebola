## This tests makes sure that the figures calculated are the same as
## the ones displayed on FTS' website. 

accuracyTest <- function(df = NULL, appeal_id = NULL) {
  cat('-------------------------------\n')
  cat('Running tests:\n')
  cat('-------------------------------\n')
  
  # list of indicators to test
  # non-cap figures are not tested at this point
  # because those aggregates are not appearing
  # as a single call from the API.
  indIDs <- c('CHD.FUN.140', 'CHD.FUN.141', 'CHD.FUN.142', 'CHD.FUN.143')
  
  # appeal id summaries
  fts_summary = fetchSummary(appeal_id)
  
  for (i in 1:length(indIDs)) {
    # extracting the max value out of
    # the timeseries respective indicator
    scraped = max(df$value[df$indID == indIDs[i]])
    
    # selecting the values depending on the indicator / test
    if (i == 1) fts = fts_summary$current_requirements
    if (i == 2) fts = fts_summary$funding
    if (i == 3) fts = round(fts_summary$funding / fts_summary$current_requirements, 3)
    if (i == 4) fts = fts_summary$pledges
    
    # testing
    if (scraped == fts) {
      cat(indIDs[i], ' | ')
      cat('Things seem wonderful!\n') 
    }
    else { 
      cat(indIDs[i], ' | ')
      cat('Spooooky!\n')
    }
  }
  cat('-------------------------------\n')
}