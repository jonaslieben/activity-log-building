
removeBeginningTimeStampFromOutlierObservations <- function(eventDataTable) {
  
  #create three new variables: one for the begin timestamp in a lubridate object, one for the end timestamp in a lubridate object and one for the duration between the two
  eventDataTableTemp <- eventDataTable %>%
    mutate(start = ymd_hms(beginningTimestamp)) %>% 
    mutate(end = ymd_hms(endTimestamp)) %>% 
    mutate(duration = as.numeric(difftime(start, end, units = "secs")))
  
  #find all the authors which contributed to the project
  authors <- eventDataTable %>% select(author) %>% distinct()
  
  for(i in 1:length(authors$author)) {
    #filter the eventdatatable for the rows of only author with index i and where the duration is a number
    eventDataTableForAuthor <- eventDataTableTemp %>% filter(author == as.character(authors$author[i])) %>% filter(!is.na(duration))
    #calculate the average and standard deviation of the duration
    averageDuration <- eventDataTableForAuthor %>% summarise(avg = mean(duration))
    SDDuration <- eventDataTableForAuthor %>% summarise(sd = sd(duration))
    
    #find all the identifiers which have a duration bigger than two times the standard deviation plus the average
    outlierIdentifiers <- eventDataTableForAuthor %>% select(identifier, duration) %>% filter(duration >  2 * SDDuration + averageDuration)
    
    for(i in 1:length(eventDataTableTemp$identifier)) {
      #assign NA to the beginning timestamp if the identifier of a row matches one of the outlierIdentifiers
      if(eventDataTable$identifier[i] %in% outlierIdentifiers) {
        eventDataTable$beginningTimestamp[i] <-NA 
      }
    }
  }

  return(eventDataTable)
}

addBeginningTimeStampsToFirstCommits <- function(eventDataTable) {
  
  #create three new variables: one for the begin timestamp in a lubridate object, one for the end timestamp in a lubridate object and one for the duration between the two
  eventDataTableTemp <- eventDataTable %>%
    mutate(start = ymd_hms(beginningTimestamp)) %>% 
    mutate(end = ymd_hms(endTimestamp)) %>% 
    mutate(duration = as.numeric(difftime(start, end, units = "secs")))
  
  #find all the authors which contributed to the project
  authors <- eventDataTable %>% select(author) %>% distinct()
  
  for(i in 1:length(authors$author)) {
    eventDataTableForAuthor <- eventDataTableTemp %>% filter(author == as.character(authors$author[i])) %>% filter(!is.na(duration))
    averageDuration <- eventDataTableForAuthor %>% summarise(avg = mean(duration))
    for(i in 1:length(eventDataTableTemp$identifier)) {
      if(is.na(eventDataTable$beginningTimestamp[i])) {
        
      }
    }
  }
}