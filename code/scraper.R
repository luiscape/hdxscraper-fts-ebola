# Scraper for CDC's Historical Ebola outbreaks data.

# dependencies
library(XML)
library(RCurl)
library(reshape2)

# Helper function for running on ScraperWiki
# Change a = T if running locally.
onSw <- function(a = F, d = 'tool/') {
  if(a == T) return(d)
  else return('')
}

# Loading helper functions.
source(onSw(), 'code/write_tables.R')

runScraper <- function() {
  ###################################################
  ###################################################
  ######### Scraping a List of Reports List #########
  ###################################################
  ###################################################
  
  # function that gets the list of documents from WHO
  # website and assembles a nice data.frame
  scrapeFTSData <- function() {
    cat('----------------------------------------\n')
    cat("Collecting the table from FTS's website.\n")
    cat('----------------------------------------\n')
  
    # CDC url
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
  
    # returning results
    cat('-------------------------------\n')
    cat('Done!\n')
    cat('-------------------------------\n')
    return(output)
  }
  
  # running
  FTSData <- scrapeFTSData()
  
  # writing output in CSV
  write.csv(cdcOutbreakList, 'data/report_list.csv', row.names = F)
  
  # writing table to scraperwiki
  writeTables(cdcOutbreakList, 'CDC_Outbreak_Data', 'scraperwiki')
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