## Function to extract indicators from
## FTS' raw inputs
exctractIndicators <- function(df = NULL) {
  cat('Extracting indicators | ')
  
  ## Extracting CHD.FUN.138 // Non-CAP Pledges
  sub <- df[df$status == 'Pledge'
            & df$is_allocation != 1, ]
  
  non_cap_pledges <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.138',
    period = row.names(tapply(sub$amount, sub$decision_date, sum)),
    value = tapply(sub$amount, sub$decision_date, sum),
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(non_cap_pledges) <- NULL  # cleaning row.names
  non_cap_pledges$value <- cumsum(non_cap_pledges$value)  # making values cumulative
  
  ## Extracting CHD.FUN.139 // Non-CAP Funding
  sub <- df[(df$status == 'Paid contribution' | df$status == 'Commitment') 
            & df$is_allocation != 1 , ]
  
  non_cap_funding <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.139',
    period = row.names(tapply(sub$amount, sub$decision_date, sum)),
    value = tapply(sub$amount, sub$decision_date, sum),
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(non_cap_funding) <- NULL  # cleaning row.names
  non_cap_funding$value <- cumsum(non_cap_funding$value)  # making values cumulative
  
  ## Extracting CHD.FUN.140 // CAP Requirements
  sub <- fetchRequirement(1060)
  
  cap_required <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.140',
    period = as.character(as.Date(sub$launch_date)),
    value = sub$current_requirements,
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(cap_required) <- NULL  # cleaning row.names
  
  ## Extracting CHD.FUN.141 // CAP Funding
  sub <- df[
      (
        (df$status == 'Paid contribution' | df$status == 'Commitment')
        & df$appeal_id == 1060
        & df$is_allocation != 1
      ) | (df$donor == 'Central Emergency Response Fund' | df$donor == 'Common Humanitarian Fund'), 
    ]
  
  cap_funding <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.141',
    period = row.names(tapply(sub$amount, sub$decision_date, sum)),
    value = tapply(sub$amount, sub$decision_date, sum),
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(cap_funding) <- NULL  # cleaning row.names
  cap_funding$value <- cumsum(cap_funding$value)  # making values cumulative
  
  ## Extracting CHD.FUN.142 // CAP Funding Coverage
  sub <- df[
      (
        (df$status == 'Paid contribution' | df$status == 'Commitment')
        & df$appeal_id == 1060
        & df$is_allocation != 1
      ) | (df$donor == 'Central Emergency Response Fund' | df$donor == 'Common Humanitarian Fund'),
    ]
  
  cap_coverage <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.142',
    period = row.names(tapply(sub$amount, sub$decision_date, sum)),
    value = tapply(sub$amount, sub$decision_date, sum),
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(cap_coverage) <- NULL  # cleaning row.names
  cap_coverage$value <- cumsum(cap_coverage$value)  # making values cumulative
  cap_coverage$value <- round((cap_coverage$value / cap_required$value), 3)  # calculating proportion
  
  ## Extracting CHD.FUN.143 // Pledges
  sub <- df[df$status == 'Pledge'
            & df$appeal_id != 0
            & df$is_allocation != 1, ]
  
  ## Using the value schema
  cap_pledges <- data.frame(
    dsID = 'fts-ebola',
    region = 'WLD',
    indID = 'CHD.FUN.143',
    period = row.names(tapply(sub$amount, sub$decision_date, sum)),
    value = tapply(sub$amount, sub$decision_date, sum),
    is_number = 1,
    source = 'http://fts.unocha.org/pageloader.aspx?page=emerg-emergencyDetails&emergID=16506'
  )
  row.names(cap_pledges) <- NULL  # cleaning row.names
  cap_pledges$value <- cumsum(cap_pledges$value)  # making values cumulative
  
  # Adding the indicators together.
  output <- rbind(cap_pledges, non_cap_pledges, non_cap_funding, cap_funding, cap_required, cap_coverage)
  
  cat('Done!\n')
  return(output)
}