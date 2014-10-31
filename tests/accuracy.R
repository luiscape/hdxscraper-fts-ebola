## This tests makes sure that the figures calculated are the same as
## the ones displayed on FTS' website. 

accuracyTest <- function(df = NULL, id = NULL) {
  cat('-------------------------------\n')
  cat('Running tests:\n')
  indIDs <- c('CHD.FUN.141', 'CHD.FUN.143')
  url = paste0('http://fts.unocha.org/api/v1/Appeal/id/', id, '.json')
  doc = fromJSON(getURL(url))
  
  for (i in 1:length(indIDs)) {
    scraped = max(df$value[df$indID == indIDs[i]])
    if (i == 1) fts = doc[[1]]$funding
    if (i == 2) fts = doc[[1]]$pledges
    if (scraped == fts) {
      cat(indIDs[i], ' | ')
      cat('Things seem wonderful!\n') 
    }
    else { 
      cat(indIDs[i], ' | ')
      cat('Spooooky!')
    }
  }
  cat('-------------------------------\n')
}