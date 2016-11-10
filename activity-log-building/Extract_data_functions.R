library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)

# get all identifiers of all branches of a certain repository on github in a vector. Inputs are the authentication, owner and repository
retrieveIdentifiersBranches <- function(authentication, owner, repository) {
  #retrieve all branches of a project
  url <- paste("https://api.github.com/repos/", owner, "/", repository, "/branches", sep = "")
  branchesJSON <- GET(url, authentication)
  
  #parse the JSON output to dataframes
  branches <- fromJSON(content(branchesJSON, "text"))
  
  #save the sha (the unique identifier of in this case a branch) in an object in order that it can be used for retrieving the commits
  shaStrings <- branches$commit$sha
  
  return (shaStrings)
}

# get all identifiers of all commits of a certain repository on github in a vector. Inputs are the authentication, owner and repository
retrieveAllCommitIdentifiers <- function(authentication, owner, repository) {
  #retrieve all identifiers of a branch
  branchIdentifiers <- retrieveIdentifiersBranches(authentication, owner, repository)
  
  for(branchIdentifier in branchIdentifiers) {
    commitIdentifiers <- vector()
    # keep an index for the pages as paging is used and only 100 commits fit onto one page
    index <- 1
    # retrieve all commit identifiers of one branch, by iterating over several pages
    repeat {
      url <- paste("https://api.github.com/repos/",owner,"/", repository, "/commits", sep ="")
      # do the API call for getting all the commits of a certain page
      commitJSON <- GET(url,query = list(per_page = 100, sha = branchIdentifier, page = index), authentication)
      # convert the JSON to R objects
      commitData <- fromJSON(content(commitJSON, "text"))
      index = index + 1
      # if there are no items anymore, stop the loop
      if(length(commitData$sha) == 0) {
        break
      } else {
        #add the identifiers to the list
        commitIdentifiersNew <- c(commitIdentifiers, commitData$sha)
        commitIdentifiers <- commitIdentifiersNew
      }
    }
  }
  
  return(commitIdentifiers)
}

# get all events of a certain repository on github in a dataframe. Inputs are the authentication, owner and repository
# the output is a dataframe containing the author (the one who made the changes), the date, the commit message, the files which were affected and the status which describe how they were affected (modified, added, removed)
extractEventData <- function(authentication, owner, repository) {
  #get all commitIdentifiers
  commitIdentifiers <- retrieveAllCommitIdentifiers(authentication, owner, repository)
  #create an empty dataframe
  eventData <- data.frame()
  #for all commitidentifiers
  for(commitIdentifier in commitIdentifiers) {
    url <- paste("https://api.github.com/repos/", owner, "/", repository, "/commits/", commitIdentifier, sep="")
    #do an API call with the url mentioned above
    commitJSON <- GET(url, authentication)
    #parse the JSON to an R object
    commitData <- fromJSON(content(commitJSON, "text"))
    
    #check the amount of files for a certain commit
    amountOfReps <- length(commitData$files$filename)
    
    #As the author, date, timestamp and message are a single observation for many files, they are duplicated, in order that the rows will still match in the dataframe
    identifier <- rep(commitIdentifier, amountOfReps)
    author <- rep(commitData$commit$author$name, amountOfReps)
    endTimestamp <- rep(paste(substr(commitData$commit$author$date, 0, 10), " _ ", substr(commitData$commit$author$date, 12, 19)), amountOfReps)
    message <- rep(commitData$commit$message, amountOfReps)
    #create the dataframe with the new event data
    newEventData <- data.frame(identifier, author, endTimestamp, message, filename = commitData$files$filename, status = commitData$files$status)
    # add the new event data to the current event data
    eventData <- rbind(eventData,newEventData)
  }
  return(eventData)
}

addBeginningTimestamp <- function(eventData) {
  tempEventData <- tbl_df(eventData)
  
  #add as a temporary beginning timestemp the endTimestamp
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
      beginningTimestamp <- authorEventData$endTimestamp[1]
    }
    
    #put the beginningTimeStamp in the dataObject
    eventData$beginningTimestamp[i] <- beginningTimestamp
  }
  
  #return the table with the beginningTimeStamp
  return(eventData)
}

