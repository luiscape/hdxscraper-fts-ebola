# Scraper for CDC's Historical Ebola outbreaks data.

# dependencies
library(XML)
library(RCurl)
library(reshape2)

# Helper function for running on ScraperWiki
# Change a = T if running locally.
onSw <- function(a = T, d = 'tool/') {
  if(a == T) return(d)
  else return('')
}

# Loading helper functions.
source(paste0(onSw(), 'code/write_tables.R'))
source(paste0(onSw(), 'code/sw_status.R'))

runScraper <- function() {
  
  ###################################################
  ###################################################
  ######### Scraping Ebola Indicators from FTS ######
  ###################################################
  ###################################################
  
  # function that gets the list of documents from WHO
  # website and assembles a nice data.frame
  scrapeFTSData <- function() {
    cat('----------------------------------------\n')
    cat("Collecting the table from FTS's website.\n")
    cat('----------------------------------------\n')
  
    # FTS Ebola url
    url = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  
    # getting the html
    doc <- htmlParse(url)
  
    # collecting the data into a data.frame
    output <- data.frame(
      date = as.character(as.Date(Sys.time())),
      CHD.FUN.138 =  xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[1]/td/table/tr[2]/td[2]', xmlValue),
      CHD.FUN.139 = xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[1]/td/table/tr[3]/td[2]', xmlValue),
      CHD.FUN.140 = xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[2]/td/table/tr[3]/td[2]', xmlValue),
      CHD.FUN.141 = xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[2]/td/table/tr[4]/td[2]', xmlValue),
      CHD.FUN.142 = xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[2]/td/table/tr[5]/td[2]', xmlValue),
      CHD.FUN.143 = xpathSApply(doc, '//*[@id="Table1"]/tr/td[2]/table/tr[2]/td/table/tr[7]/td[2]/i[1]', xmlValue)
      )
    
    # Melting
    outMelt <- suppressWarnings(melt(output, id = "date"))
    
    ## Cleaning
    # Removing spaces
    outMelt$value <- sub('         \r\n    ', '', outMelt$value)
    outMelt$value <- sub('\r\n    ', '', outMelt$value)
    outMelt$value <- sub('million', '', outMelt$value)
    outMelt$value <- sub('USD', '', outMelt$value)
    outMelt$value <- sub('%', '', outMelt$value)
    outMelt$value <- as.numeric(outMelt$value)
    
    # Multiplying for 1M
    outMelt$value[1:4] <- outMelt$value[1:4] * 1e+06
    outMelt$value[6] <- outMelt$value[6] * 1e+06
    outMelt$value <- format(outMelt$value, scientific = F)
    outMelt$value <- sub('\\.0', '', outMelt$value)
    outMelt$value[5] <- sub('       ', '', outMelt$value[5])  # mystery: spaces appearing
    

    # returning results
    cat('-------------------------------\n')
    cat('Scraping done!\n')
    cat('-------------------------------\n')
    return(outMelt)
  }
  
  # running
  FTSData <- scrapeFTSData()
  
  parseCPSFormat <- function(df) {
    ## Indicator schema:
    # indID
    # name
    # units
    indicator <- read.csv(paste0(onSw(), 'data/source/indicator.csv'))
    
    ## Value schema:
    # dsID
    # region
    # indID
    # period
    # value
    # is_number
    # source
    value <- data.frame(
      indID = df$variable,
      period = df$date,
      value = df$value,
      dsID = 'fts-ebola',
      region = 'WLD',
      is_number = 1,
      source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
    )
    
    ## Dataset schema:
    # dsID
    # last_updated
    # last_scraped
    # name
    dataset <- data.frame(
      dsID = 'fts',
      last_updated = max(as.Date(value$period)),
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
  parseCPSFormat(FTSData)
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