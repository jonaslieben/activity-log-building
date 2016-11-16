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
  branches <- fromJSON(httr::content(branchesJSON, as = "text"))
  
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
      commitData <- fromJSON(httr::content(commitJSON, "text"))
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
    commitData <- fromJSON(httr::content(commitJSON, "text"))
    
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
  #save endtimestamp as lubridate object
  eventData$endTimestamp <- ymd_hms(eventData$endTimestamp)
  return(eventData)
}


