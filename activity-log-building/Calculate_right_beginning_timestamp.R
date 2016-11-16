
removeBeginningTimeStampFromOutlierObservations <- function(eventDataTable) {
  
  #create three new variables: one for the begin timestamp in a lubridate object, one for the end timestamp in a lubridate object and one for the duration between the two
  
  eventDataTableTemp <- eventDataTable %>%
    filter(!is.na(beginningTimestamp)) %>%
    
    mutate(duration = as.numeric(difftime(beginningTimestamp, endTimestamp, units = "secs")))
  
  #find all the authors which contributed to the project
  authors <- eventDataTable %>% select(author) %>% distinct()
  
  for(i in 1:length(authors$author)) {
    #filter the eventdatatable for the rows of only author with index i and where the duration is a number
    eventDataTableForAuthor <- eventDataTableTemp %>% filter(author == as.character(authors$author[i])) %>% filter(!is.na(duration))
    #calculate the average and standard deviation of the duration
    averageDuration <- eventDataTableForAuthor %>% summarise(avg = mean(duration))
    SDDuration <- eventDataTableForAuthor %>% summarise(sd = sd(duration))
    
    cutoff <- averageDuration + SDDuration * 2
    #find all the identifiers which have a duration bigger than two times the standard deviation plus the average
    outlierIdentifiers <- eventDataTableForAuthor %>% 
      select(identifier, duration) %>% 
      filter(duration >  cutoff$avg[1])
    
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
    mutate(duration = as.numeric(difftime(beginningTimestamp, endTimestamp, units = "secs")))
  
  #find all the authors which contributed to the project
  authors <- eventDataTable %>% select(author) %>% distinct()
  
  
  for(i in 1:length(authors$author)) {
    eventDataTableForAuthor <- eventDataTableTemp %>% filter(author == as.character(authors$author[i])) %>% filter(!is.na(duration))
    averageDuration <- eventDataTableForAuthor %>% summarise(avg = mean(duration))
    for(j in 1:length(eventDataTableTemp$identifier)) {
      if(is.na(eventDataTableTemp$beginningTimestamp[j]) && eventDataTableTemp$author[j] == as.character(authors$author[i])) {
        end <- eventDataTableTemp$endTimestamp[j]
        begin <- end - dseconds(as.numeric(averageDuration))
        eventDataTableTemp$beginningTimestamp[j] <- begin
      }
    }
  }
  
  return(eventDataTableTemp)
}

addProvisionalBeginningTimestamp <- function(eventData) {
  
  tempEventData <- tbl_df(eventData)
  
  #add as a temporary beginning timestemp the endTimestamp
  eventData$beginningTimestamp <- tempEventData$endTimestamp
  tempEventData$beginningTimestamp <- tempEventData$endTimestamp
  
  #save the amount of records in a new variable
  amountOfRecords <- length(eventData$author)
  
  #for each row, put a beginning timestamp using the data manipulation techniques described below
  for(i in 1:amountOfRecords) {
    #save the identifier and author. The unlisting is done in order to be able to make comparisons
    identifierAtIndex <- tempEventData %>% select(identifier) %>% slice(i)
    identifierAtIndex <- unlist(identifierAtIndex)
    authorAtIndex <- tempEventData %>% select(author) %>% slice(i)
    authorAtIndex <- unlist(authorAtIndex)
    
    #make a table containing only the identifiers and endTimeStamp of the author, who did the commit of record i, and sort all rows on endTimestamp in descending order
    authorEventData <- tempEventData %>% filter(authorAtIndex == author) %>% arrange(desc(endTimestamp)) %>% select(endTimestamp,identifier) %>% distinct()
    #look up the index of the row of the AuthorEventData object with the identifier which is saved earlier
    indexAuthorEventData <- match(identifierAtIndex, authorEventData$identifier)
    #the index of the beginning timestamp is just the row before in the authorEventData table
    indexPreviousRowAuthorEventData <- indexAuthorEventData - 1
    
    #if there is a previous row, take the endtimestamp of the previous activity executed by the author as the beginning timestamp
    #if there is no previous row, take the endtimestamp of the current activity as the beginning timestamp
    if(indexPreviousRowAuthorEventData != 0) {
      beginningTimestamp <- authorEventData$endTimestamp[indexPreviousRowAuthorEventData]
    } else {
      beginningTimestamp <- NA
    }
    #put the beginningTimeStamp in the dataObject
    eventData$beginningTimestamp[i] <- beginningTimestamp
  }
  
  #return the table with the beginningTimeStamp
  return(eventData)
}
